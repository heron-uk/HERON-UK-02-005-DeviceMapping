#' Classify Medical Devices by Eligibility, Regulatory Class, and Clinical Specialty
#'
#' @description
#' Uses an \pkg{ellmer} `Chat` object (e.g., created with `ellmer::chat_openai()`
#' or `ellmer::chat_ollama()`) to first determine whether unique `device_name`
#' values in a tibble are truly medical devices (`"yes"` or `"no"`), excluding
#' non-device dm+d items such as creams, pharmaceuticals, and nutritional feeds.
#' True medical devices are then classified into regulatory risk classes
#' (`Class I`, `Class IIa`, `Class IIb`, `Class III`) and clinical specialties.
#' Groups concepts into batches of `batch_size` using an ID-anchored
#' `ellmer::type_array()` schema, retries missing or failed items up to
#' `max_retries`, reports live concept, error, and cumulative API cost via
#' \pkg{cli}, and returns `NA_character_` for any concepts that still fail
#' (or for class/specialty when `is_medical_device == "no"`).
#'
#' @param chat An \pkg{ellmer} `Chat` object (such as from
#'   `ellmer::chat_openai(model = "gpt-4o-mini")` or
#'   `ellmer::chat_ollama(model = "qwen2.5:7b", api_args = list(temperature = 0))`).
#' @param data A `data.frame` or `tibble` containing a `device_name` column.
#' @param max_retries Integer scalar. Maximum number of attempts for any concept
#'   before returning `NA_character_`. Defaults to `3L`.
#' @param batch_size Integer scalar. Number of unique device names to classify
#'   per LLM request. Defaults to `20L`.
#' @param show_cost Logical scalar. Whether to track and display cumulative API
#'   costs via `chat$get_cost()` during execution. Defaults to `TRUE`.
#'
#' @return A `tibble` with the original rows and columns of `data`, plus:
#'   \itemize{
#'     \item `is_medical_device`: Character (`"yes"`, `"no"`, or `NA_character_`).
#'     \item `device_class`: Character (`"Class I"`, `"Class IIa"`,
#'       `"Class IIb"`, `"Class III"`, or `NA_character_` if not a device or failed).
#'     \item `medical_specialty`: Character (one of the 17 standard medical
#'       specialties, or `NA_character_` if not a device or failed).
#'   }
#'
#' @importFrom cli cli_abort cli_alert_info cli_alert_success cli_alert_warning cli_progress_bar cli_progress_cleanup cli_progress_done cli_progress_update pb_bar pb_current pb_eta pb_extra pb_percent pb_total
#' @importFrom dplyr as_tibble distinct filter left_join mutate pull select tibble
#' @importFrom ellmer type_array type_enum type_integer type_object type_string
#' @importFrom purrr list_rbind
#' @importFrom rlang .data
#' @importFrom stringr str_squish
#' @export
classify_medical_devices <- function(chat,
                                     data,
                                     max_retries = 3L,
                                     batch_size = 20L,
                                     show_cost = TRUE) {
  
  cli::cli_progress_cleanup()
  
  if (!inherits(chat, "Chat")) {
    cli::cli_abort(c(
      "{.arg chat} must be an {.pkg ellmer} {.cls Chat} object.",
      "i" = "Create one with e.g. {.code ellmer::chat_ollama(model = \"qwen2.5:7b\")}."
    ))
  }
  
  if (!is.data.frame(data) || !"device_name" %in% names(data)) {
    cli::cli_abort(
      "{.arg data} must be a data.frame or tibble containing a {.col device_name} column."
    )
  }
  
  max_retries <- as.integer(max_retries)
  if (length(max_retries) != 1L || is.na(max_retries) || max_retries < 1L) {
    cli::cli_abort("{.arg max_retries} must be a single positive integer.")
  }
  
  batch_size <- as.integer(batch_size)
  if (length(batch_size) != 1L || is.na(batch_size) || batch_size < 1L) {
    cli::cli_abort("{.arg batch_size} must be a single positive integer.")
  }
  
  allowed_is_device <- c("yes", "no")
  allowed_classes <- c("Class I", "Class IIa", "Class IIb", "Class III")
  allowed_specialties <- c(
    "Anesthesiology",
    "Cardiovascular",
    "Chemistry & Toxicology",
    "Dental",
    "Ear, Nose, and Throat (ENT)",
    "Gastroenterology and Urology",
    "General and Plastic Surgery",
    "General Hospital",
    "Hematology & Pathology",
    "Immunology & Microbiology",
    "Neurology",
    "Obstetrical and Gynecological",
    "Ophthalmic",
    "Orthopedic",
    "Physical Medicine",
    "Radiology",
    "Other"
  )
  
  batch_schema <- ellmer::type_object(
    "Batch classification results for a numbered list of candidate medical device items.",
    items = ellmer::type_array(
      description = "One classification object for each numbered item in the prompt.",
      items = ellmer::type_object(
        id = ellmer::type_integer(
          description = "The exact integer ID (1, 2, ...) matching the input list."
        ),
        is_medical_device = ellmer::type_enum(
          values = allowed_is_device,
          description = "'yes' if the item is truly a regulated medical device; 'no' if it is a pharmaceutical, topical cream/emollient, nutritional feed, cosmetic, or other non-device product."
        ),
        device_type = ellmer::type_string(
          description = "3 to 6 word summary of what the item is (e.g., 'breast reconstruction tissue expander', 'topical emollient cream (non-device)')."
        ),
        medical_specialty = ellmer::type_enum(
          values = c(allowed_specialties, "Not Applicable"),
          description = "Primary medical specialty category for the device, or 'Not Applicable' if is_medical_device is 'no'."
        ),
        device_class = ellmer::type_enum(
          values = c(allowed_classes, "Not Applicable"),
          description = "Regulatory risk class (Class I, Class IIa, Class IIb, or Class III), or 'Not Applicable' if is_medical_device is 'no'."
        )
      )
    )
  )
  
  base_chat <- chat$clone()
  base_chat$set_turns(list())
  base_chat$set_system_prompt(.device_classifier_system_prompt())
  
  work_df <- dplyr::as_tibble(data) |>
    dplyr::mutate(.clean_name = stringr::str_squish(as.character(.data$device_name)))
  
  unique_concepts <- work_df |>
    dplyr::filter(!is.na(.data$.clean_name), .data$.clean_name != "") |>
    dplyr::distinct(.data$.clean_name) |>
    dplyr::pull(.data$.clean_name)
  
  n_total <- nrow(work_df)
  n_unique <- length(unique_concepts)
  
  if (n_unique == 0L) {
    cli::cli_alert_warning(
      "No non-empty {.col device_name} values found in {.arg data}. Returning {.val NA} columns."
    )
    return(
      work_df |>
        dplyr::select(-".clean_name") |>
        dplyr::mutate(
          is_medical_device = NA_character_,
          device_class = NA_character_,
          medical_specialty = NA_character_
        )
    )
  }
  
  batches <- split(
    unique_concepts,
    ceiling(seq_along(unique_concepts) / batch_size)
  )
  
  cli::cli_alert_info(
    "Found {.val {n_unique}} unique concept{?s} across {.val {n_total}} row{?s} ({.val {length(batches)}} batch{?es} of up to {.val {batch_size}}; max retries: {.val {max_retries}})."
  )
  
  extract_call_cost <- function(ch) {
    if (!isTRUE(show_cost) || !is.function(ch$get_cost)) {
      return(NA_real_)
    }
    cost_val <- tryCatch(
      as.numeric(ch$get_cost(include = "all")),
      error = function(e) NA_real_
    )
    if (length(cost_val) == 1L && !is.na(cost_val)) cost_val else NA_real_
  }
  
  format_cost <- function(cost_num) {
    if (is.na(cost_num)) {
      return("N/A")
    }
    if (cost_num > 0 && cost_num < 0.01) {
      return(sprintf("$%.4f", cost_num))
    }
    sprintf("$%.2f", cost_num)
  }
  
  classify_batch <- function(concepts_vec) {
    n_b <- length(concepts_vec)
    out_is_device <- rep(NA_character_, n_b)
    out_class <- rep(NA_character_, n_b)
    out_spec <- rep(NA_character_, n_b)
    pending_idx <- seq_len(n_b)
    batch_cost <- 0
    has_cost_data <- FALSE
    
    for (attempt in seq_len(max_retries)) {
      if (length(pending_idx) == 0L) break
      
      curr_concepts <- concepts_vec[pending_idx]
      prompt_lines <- paste0(seq_along(curr_concepts), ": ", curr_concepts)
      prompt_text <- paste(
        "Evaluate each of the following numbered items, determine whether it is truly a medical device ('yes' or 'no'), and return the matching id for every item:",
        paste(prompt_lines, collapse = "\n"),
        sep = "\n"
      )
      
      ch <- base_chat$clone()
      res <- tryCatch(
        {
          ch$chat_structured(
            prompt_text,
            type = batch_schema,
            echo = "none"
          )
        },
        error = function(e) NULL
      )
      
      attempt_cost <- extract_call_cost(ch)
      if (!is.na(attempt_cost)) {
        batch_cost <- batch_cost + attempt_cost
        has_cost_data <- TRUE
      }
      
      items_df <- if (is.list(res) && !is.null(res$items)) res$items else NULL
      
      if (is.data.frame(items_df) &&
          all(c("id", "is_medical_device", "device_class", "medical_specialty") %in% names(items_df))) {
        for (r in seq_len(nrow(items_df))) {
          item_id <- suppressWarnings(as.integer(items_df$id[[r]]))
          is_dev <- tolower(as.character(items_df$is_medical_device[[r]]))
          d_cls <- as.character(items_df$device_class[[r]])
          m_spc <- as.character(items_df$medical_specialty[[r]])
          
          if (!is.na(item_id) &&
              item_id >= 1L &&
              item_id <= length(curr_concepts) &&
              !is.na(is_dev) && is_dev %in% allowed_is_device) {
            orig_pos <- pending_idx[[item_id]]
            
            if (is_dev == "no") {
              out_is_device[[orig_pos]] <- "no"
              out_class[[orig_pos]] <- NA_character_
              out_spec[[orig_pos]] <- NA_character_
            } else if (!is.na(d_cls) && d_cls %in% allowed_classes &&
                       !is.na(m_spc) && m_spc %in% allowed_specialties) {
              out_is_device[[orig_pos]] <- "yes"
              out_class[[orig_pos]] <- d_cls
              out_spec[[orig_pos]] <- m_spc
            }
          }
        }
      }
      
      pending_idx <- which(
        is.na(out_is_device) |
          (out_is_device == "yes" & (is.na(out_class) | is.na(out_spec)))
      )
      
      if (length(pending_idx) > 0L && attempt < max_retries) {
        Sys.sleep(0.5 * attempt)
      }
    }
    
    list(
      df = dplyr::tibble(
        .clean_name = concepts_vec,
        is_medical_device = out_is_device,
        device_class = out_class,
        medical_specialty = out_spec
      ),
      cost = if (has_cost_data) batch_cost else NA_real_
    )
  }
  
  n_processed <- 0L
  n_errors <- 0L
  total_cost <- 0
  any_cost_available <- FALSE
  cost_str <- if (isTRUE(show_cost)) "$0.00" else "N/A"
  results_list <- vector("list", length(batches))
  
  # Compact format so cost stays visible even in narrow RStudio/Positron panes
  pb_format <- paste0(
    "{cli::pb_bar} {cli::pb_percent} | ",
    "{cli::pb_current}/{cli::pb_total} | ",
    if (isTRUE(show_cost)) "{cli::pb_extra$cost} | " else "",
    "Err: {cli::pb_extra$errors} | ",
    "ETA: {cli::pb_eta}"
  )
  
  pb_id <- cli::cli_progress_bar(
    total = n_unique,
    format = pb_format,
    extra = list(cost = cost_str, errors = n_errors),
    clear = TRUE,
    auto_terminate = TRUE
  )
  
  on.exit(
    cli::cli_progress_done(id = pb_id, result = "clear"),
    add = TRUE
  )
  
  for (b_idx in seq_along(batches)) {
    batch_out <- classify_batch(batches[[b_idx]])
    batch_res <- batch_out$df
    results_list[[b_idx]] <- batch_res
    
    if (!is.na(batch_out$cost)) {
      total_cost <- total_cost + batch_out$cost
      any_cost_available <- TRUE
      cost_str <- format_cost(total_cost)
    } else if (!any_cost_available) {
      cost_str <- "N/A"
    }
    
    n_processed <- n_processed + nrow(batch_res)
    batch_errors <- sum(
      is.na(batch_res$is_medical_device) |
        (batch_res$is_medical_device == "yes" &
           (is.na(batch_res$device_class) | is.na(batch_res$medical_specialty)))
    )
    n_errors <- n_errors + batch_errors
    
    cli::cli_progress_update(
      set = n_processed,
      extra = list(cost = cost_str, errors = n_errors),
      id = pb_id,
      force = (n_processed == n_unique)
    )
  }
  
  cli::cli_progress_done(id = pb_id)
  on.exit(NULL)
  
  lookup <- purrr::list_rbind(results_list)
  n_succeeded <- n_unique - n_errors
  
  final_cost_msg <- if (isTRUE(show_cost) && any_cost_available) {
    paste0(" | Total cost: ", format_cost(total_cost))
  } else {
    ""
  }
  
  if (n_errors == 0L) {
    cli::cli_alert_success(
      "Processed {.val {n_unique}}/{.val {n_unique}} unique concept{?s} ({.val {n_succeeded}} succeeded, {.val 0} errors{final_cost_msg})."
    )
  } else {
    cli::cli_alert_warning(
      "Processed {.val {n_unique}}/{.val {n_unique}} unique concept{?s} ({.val {n_succeeded}} succeeded, {.val {n_errors}} error{?s} set to {.val NA}{final_cost_msg})."
    )
  }
  
  work_df |>
    dplyr::left_join(lookup, by = ".clean_name") |>
    dplyr::select(-".clean_name")
}

#' Internal System Prompt for Medical Device Classification
#'
#' @return Character scalar.
#' @noRd
.device_classifier_system_prompt <- function() {
  paste(
    "You are an expert EU/UK MDR and FDA medical device regulatory specialist.",
    "You will receive a numbered list of product names from routinely collected clinical data (e.g., NHS dm+d, hospital procurement, or registry records, which may include brand names, manufacturer names, catalog abbreviations, or non-device products).",
    "For EVERY numbered item in the list, return an object with:",
    "  1. `id`: The exact integer ID matching the input line.",
    "  2. `is_medical_device`: 'yes' if the item is truly a regulated medical device, or 'no' if it is NOT a medical device.",
    "  3. `device_type`: A concise 3-6 word description identifying what the item is.",
    "  4. `medical_specialty`: The regulatory specialty category (choose strictly from the 17 allowed values), or 'Not Applicable' if `is_medical_device` is 'no'.",
    "  5. `device_class`: The EU/UK MDR regulatory risk class (`Class I`, `Class IIa`, `Class IIb`, or `Class III`), or 'Not Applicable' if `is_medical_device` is 'no'.",
    "Evaluate each item independently—never let adjacent items in a batch bias the current item.",
    "",
    "### PART 0: MEDICAL DEVICE ELIGIBILITY (`is_medical_device`)",
    "- Return `is_medical_device: 'no'` (with `medical_specialty: 'Not Applicable'` and `device_class: 'Not Applicable'`) for:",
    "  * Topical creams, ointments, emollients, lotions, gels, bath additives, shampoos, barrier creams, and sunscreens (e.g., Diprobase, Epaderm, Aveeno, Aqueous cream, Hydrocortisone cream).",
    "  * Pharmaceuticals and medicinal products: tablets, capsules, oral solutions, injections, IV drug infusions, vaccines, eye/ear/nasal drops, inhalers containing drugs, lozenges, suppositories.",
    "  * Nutritional and dietary products: oral nutritional supplements, enteral feeds/formulas (e.g., Ensure, Fortisip), infant formulas, thickeners, gluten-free foods, vitamins/minerals.",
    "  * Cosmetics, toiletries, soaps, toothpastes, mouthwashes, and general non-medical consumables.",
    "- Return `is_medical_device: 'yes'` for true regulated medical devices whose primary mode of action is physical/mechanical/diagnostic rather than pharmacological/metabolic, including: implants, surgical instruments, catheters, syringes/needles, wound dressings/bandages, ostomy/incontinence appliances, compression hosiery, diagnostic test strips/meters, ventilators, imaging systems, and internal surgical hemostats/sealants (e.g., Floseal, Surgicel, Tisseel).",
    "",
    "### PART 1: MEDICAL SPECIALTY RULES (Classify by Regulatory Anatomy/Function, Not Surgeon Type)",
    "- Anesthesiology: ICU/transport ventilators, anesthesia workstations, vaporizers, endotracheal tubes, epidural needles, pulse oximeters/capnographs.",
    "- Cardiovascular: Pacemakers, ICDs, cardiac resynchronization devices, heart valves, vascular/coronary stents, angioplasty balloons, cardiac guidewires, ablation catheters, hemodynamic/ECG monitors, vascular grafts (e.g., Medtronic Cardiac, Edwards, Boston Scientific, Abbott Vascular).",
    "- Chemistry & Toxicology: Clinical chemistry analyzers, blood gas analyzers, blood glucose meters/strips, electrolyte/immunoassay chemistry analyzers, drug/toxicological screening tests.",
    "- Dental: Dental implants, dental handpieces, orthodontic brackets/wires, dental amalgam/composites, synthetic dental bone grafts, endodontic files.",
    "- Ear, Nose, and Throat (ENT): Hearing aids, cochlear implants, tympanostomy tubes, nasal dilators/packing, laryngeal mask airways (LMAs), tracheostomy tubes, ENT endoscopes.",
    "- Gastroenterology and Urology: GI endoscopes, colonoscopes, ERCP catheters, lithotripters, Foley/urinary catheters, ureteral stents, nephrostomy tubes, enteral feeding tubes (PEG/NG), ostomy bags/appliances.",
    "- General and Plastic Surgery: Breast implants, breast tissue expanders, dermal fillers, surgical mesh (hernia/soft-tissue), sutures, skin/internal staplers, wound dressings/NPWT, electrosurgical generators/pencils, laparoscopic trocars, scalpels, internal absorbable hemostatic agents and surgical sealants (e.g., Mentor, Allergan/Natrelle, Ethicon, Floseal, Surgicel, Tisseel).",
    "- General Hospital: Hospital beds, infusion pumps, syringe pumps, IV administration sets/cannulas, clinical thermometers, hypodermic syringes/needles, patient lifts, sharps containers.",
    "- Hematology & Pathology: Coagulation analyzers, automated blood cell counters (CBC), blood bank centrifuges, tissue processors, microtomes, histology slide stainers.",
    "- Immunology & Microbiology: Viral/bacterial serological tests, PCR/NAT infectious disease assays, blood culture media, microbiology susceptibility plates, antibody/antigen detection kits.",
    "- Neurology: Devices interacting directly with brain, spinal cord tissue, nerves, or CSF: deep brain stimulators (DBS), spinal cord/vagus nerve stimulators, CSF shunts, intracranial pressure monitors, aneurysm coils/flow diverters, EEGs, intraoperative neuromonitoring — NEVER structural spinal bone screws, rods, or fusion cages.",
    "- Obstetrical and Gynecological: Fetal monitors (CTG), contraceptive IUDs, colposcopes, vaginal speculums, obstetrical forceps/vacuum extractors, hysteroscopes, endometrial ablation devices.",
    "- Ophthalmic: Contact lenses, intraocular lenses (IOLs), phacoemulsification systems, ophthalmic surgical lasers, tonometers, viscoelastics, ophthalmic surgical knives.",
    "- Orthopedic: STRICTLY musculoskeletal, bone, joint, and structural spine hardware (including spine implants placed by neurosurgeons): total/partial hip, knee, shoulder, or ankle joint replacements; bone fixation plates, screws, and intramedullary nails; pedicle screw/rod systems; spinal interbody fusion cages and disc prostheses; bone cement; arthroscopy instruments (e.g., DePuy Synthes, Stryker, Zimmer Biomet, NuVasive, Globus Medical, Smith+Nephew).",
    "- Physical Medicine: Powered/manual wheelchairs, TENS units, neuromuscular electrical stimulators, traction equipment, external limb prostheses/orthoses, crutches/walkers.",
    "- Radiology: CT scanners, MRI systems, X-ray machines, fluoroscopy C-arms, diagnostic ultrasound systems, mammography units, nuclear medicine/PET scanners, radiation therapy linear accelerators — NOT catheters or stents placed under imaging guidance.",
    "- Other: General facility equipment, software, or non-specific consumables that cannot be mapped to any of the 16 clinical specialties above.",
    "",
    "### PART 2: EU/UK MDR RISK CLASS RULES (`device_class`)",
    "- Class I: ONLY non-invasive devices touching intact skin or simple reusable surgical hand instruments (e.g., hospital beds, wheelchairs, manual scalpels, forceps, stethoscopes, external intact-skin bandages, vaginal speculums). NEVER assign Class I to any implantable, absorbable, sterile invasive, or biological/animal-derived surgical agent.",
    "- Class IIa: Short-term invasive devices (<30 days), simple vascular/urinary access tubing, dental fillings, and standard active diagnostic/monitoring devices (e.g., hypodermic syringes, IV cannulas, Foley catheters, laryngeal masks, hearing aids, diagnostic ultrasound, ECG/fetal monitors, contact lenses).",
    "- Class IIb: Long-term implantable osteosynthesis/soft-tissue devices, active therapeutic/life-support devices, and ionizing radiology equipment (e.g., bone plates/screws, pedicle screw/rod systems, intramedullary nails, synthetic surgical mesh, intraocular lenses, ureteral stents, ICU ventilators, infusion pumps, electrosurgical units, X-ray/CT/MRI scanners, blood bags).",
    "- Class III: Highest-risk life-sustaining, absorbable, or biological/medicinal devices (EU MDR Rules 8, 14, 18): all cardiovascular implants (pacemakers, ICDs, heart valves, coronary/vascular stents, aneurysm coils), CNS/brain implants (deep brain stimulators, CSF shunts), total/partial joint replacements (hip, knee, shoulder), spinal interbody cages and total disc replacements, breast implants and breast tissue expanders, contraceptive IUDs, and internal absorbable surgical hemostats, sealants, or collagen/gelatin/thrombin matrices (e.g., Floseal, Surgicel, Tisseel, Tachosil).",
    "",
    "### PART 3: CALIBRATION EXAMPLES",
    "- \"Diprobase cream\" -> is_medical_device: \"no\", device_type: \"Topical emollient cream (non-device)\", medical_specialty: \"Not Applicable\", device_class: \"Not Applicable\"",
    "- \"Ensure Plus milkshake style liquid\" -> is_medical_device: \"no\", device_type: \"Oral nutritional supplement drink (non-device)\", medical_specialty: \"Not Applicable\", device_class: \"Not Applicable\"",
    "- \"MENTOR CPX 2 Breast Tissue Expander\" -> is_medical_device: \"yes\", device_type: \"Breast reconstruction tissue expander implant\", medical_specialty: \"General and Plastic Surgery\", device_class: \"Class III\"",
    "- \"ReLine (Nuvasive, Inc.)\" -> is_medical_device: \"yes\", device_type: \"Posterior spinal pedicle screw fixation system\", medical_specialty: \"Orthopedic\", device_class: \"Class IIb\"",
    "- \"Floseal Hemostatic Matrix\" -> is_medical_device: \"yes\", device_type: \"Absorbable bovine gelatin thrombin surgical hemostat\", medical_specialty: \"General and Plastic Surgery\", device_class: \"Class III\"",
    "- \"Stryker Triathlon CR Femoral Component\" -> is_medical_device: \"yes\", device_type: \"Total knee replacement femoral implant\", medical_specialty: \"Orthopedic\", device_class: \"Class III\"",
    "- \"BD Alaris GP Volumetric Pump\" -> is_medical_device: \"yes\", device_type: \"Intravenous fluid infusion pump\", medical_specialty: \"General Hospital\", device_class: \"Class IIb\"",
    "- \"Medtronic Micra AV\" -> is_medical_device: \"yes\", device_type: \"Leadless implantable cardiac pacemaker\", medical_specialty: \"Cardiovascular\", device_class: \"Class III\"",
    sep = "\n"
  )
}
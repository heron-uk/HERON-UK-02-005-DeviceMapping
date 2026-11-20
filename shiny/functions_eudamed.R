get_eudamed_device <- function(udi, max_active = 10, max_batch_retries = 2, retry_delay = 3) {
  unique_udi <- unique(as.character(udi))
  n_udis <- length(unique_udi)
  
  cli::cli_inform(c("i" = "Preparing to query {.val {n_udis}} unique UDI{?s} from EUDAMED."))
  
  # Internal helper to run a single batch pass
  run_batch <- function(udi_subset, active_conns, pass_label = "Fetching EUDAMED devices") {
    reqs <- purrr::map(udi_subset, build_eudamed_request)
    resps <- httr2::req_perform_parallel(
      reqs,
      on_error = "continue",
      max_active = active_conns,
      progress = list(
        name = pass_label,
        format = paste0("{cli::pb_spin} ", pass_label, " {cli::pb_current}/{cli::pb_total} [{cli::pb_bar}] {cli::pb_percent} | ETA: {cli::pb_eta}"),
        clear = FALSE
      )
    )
    
    purrr::map2(
      resps,
      udi_subset,
      \(resp, udi_val) parse_eudamed_response(resp, udi_val) |>
        dplyr::mutate(query_udi_di = udi_val),
      .progress = list(
        name = "Parsing responses",
        format = "{cli::pb_spin} Parsing JSON {cli::pb_current}/{cli::pb_total} [{cli::pb_bar}] {cli::pb_percent}",
        clear = FALSE
      )
    ) |>
      purrr::list_rbind()
  }
  
  # Initial pass
  results <- run_batch(unique_udi, active_conns = max_active)
  
  # Automatic retry loop for any "failed" requests
  attempt <- 1
  while (attempt <= max_batch_retries) {
    failed_udis <- results |>
      dplyr::filter(status == "failed") |>
      dplyr::pull(query_udi_di) |>
      unique()
    
    if (length(failed_udis) == 0) break
    
    # Reduce concurrency on retries (e.g., 10 -> 5 -> 2) to ease server load
    retry_conns <- max(1, floor(max_active / (2 ^ attempt)))
    
    cli::cli_alert_warning(
      "Retrying {.val {length(failed_udis)}} failed request{?s} (Attempt {attempt}/{max_batch_retries}, max_active = {retry_conns}) after {retry_delay}s pause..."
    )
    Sys.sleep(retry_delay)
    
    retry_res <- run_batch(
      failed_udis,
      active_conns = retry_conns,
      pass_label = paste0("Retry #", attempt)
    )
    
    # Swap out old failed rows for the newly retried rows
    results <- results |>
      dplyr::filter(status != "failed") |>
      dplyr::bind_rows(retry_res)
    
    attempt <- attempt + 1
  }
  
  # Final summary
  n_found <- sum(results$status == "found", na.rm = TRUE)
  n_not_found <- sum(results$status == "not_found", na.rm = TRUE)
  n_failed <- sum(results$status == "failed", na.rm = TRUE)
  
  cli::cli_alert_success(
    "Completed: {.val {n_found}} device record{?s} retrieved ({.val {n_not_found}} UDI{?s} not found, {.val {n_failed}} request{?s} failed)."
  )
  
  return(results)
}
build_eudamed_request <- function(udi_di) {
  request("https://api.datalake.sante.service.ec.europa.eu/eudamed/udi") |>
    req_url_query(
      PRIMARY_DI = udi_di,
      format = "json",
      `api-version` = "v1.0"
    ) |>
    req_retry(max_tries = 3)
}
parse_eudamed_response <- function(resp, udi_di) {
  # Helper to generate an NA row with a specific status flag
  make_empty_row <- function(status_flag) {
    tibble::tibble(
      udi = udi_di,
      status = status_flag,
      catalogue_number = NA_character_,
      basic_udi = NA_character_,
      device_name = NA_character_,
      nomenclature_code = NA_character_,
      manufacturer = NA_character_
    )
  }
  
  # Case 1: Request failed (network error, timeout, or HTTP 4xx/5xx error)
  if (!inherits(resp, "httr2_response") || resp_is_error(resp)) {
    return(make_empty_row("failed"))
  }
  
  # Safely parse JSON in case of malformed payload
  device <- tryCatch(
    resp_body_json(resp),
    error = \(e) NULL
  )
  
  if (is.null(device)) {
    return(make_empty_row("failed"))
  }
  
  # Case 2: Request succeeded, but UDI was not found in EUDAMED
  if (length(device$value) == 0) {
    return(make_empty_row("not_found"))
  }
  
  # Case 3: UDI found
  purrr::map(device$value, \(item) {
    tibble::tibble(
      udi = udi_di,
      status = "found",
      trade_name = purrr::pluck(item, "TRADE_NAME", .default = NA_character_),
      device_name = purrr::pluck(item, "DEVICE_NAME", .default = NA_character_),
      catalogue_number = purrr::pluck(item, "REFERENCE", .default = NA_character_),
      basic_udi = purrr::pluck(item, "BASIC_UDI", .default = NA_character_),
      nomenclature_code = purrr::pluck(item, "NOMENCLATURE_CODE", .default = NA_character_),
      manufacturer = purrr::pluck(item, "MF_NAME", .default = NA_character_)
    )
  }) |>
    purrr::list_rbind()
}

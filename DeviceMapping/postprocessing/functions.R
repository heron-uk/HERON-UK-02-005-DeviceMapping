prepareResult <- function(result, resultList) {
  purrr::map(resultList, \(x) filterResult(result, x))
}

filterResult <- function(result, filt) {

  q <- function(nm) {
    paste0(".data$", nm, " %in% filt[[\"", nm, "\"]]") |>
      rlang::parse_exprs() |>
      rlang::eval_tidy()
  }

  # filter columns
  cols <- c("cdm_name", "variable_name", "variable_level", "estimate_name")
  cols <- cols[cols %in% names(filt)]
  for (nm in cols) {
    result <- dplyr::filter(result, !!!q(nm))
  }

  # filter settings
  cols <- c(
    "result_id", "result_type", "package_name", "package_version",
    "min_cell_count", omopgenerics::settingsColumns(result = result)
  )
  cols <- cols[cols %in% names(filt)]
  for (nm in cols) {
    result <- omopgenerics::filterSettings(result, !!!q(nm))
  }

  # filter group
  cols <- omopgenerics::groupColumns(result = result)
  cols <- cols[cols %in% names(filt)]
  for (nm in cols) {
    result <- omopgenerics::filterGroup(result, !!!q(nm))
  }

  # filter strata
  cols <- omopgenerics::strataColumns(result = result)
  cols <- cols[cols %in% names(filt)]
  for (nm in cols) {
    result <- omopgenerics::filterStrata(result, !!!q(nm))
  }

  # filter additional
  cols <- omopgenerics::additionalColumns(result = result)
  cols <- cols[cols %in% names(filt)]
  for (nm in cols) {
    result <- omopgenerics::filterAdditional(result, !!!q(nm))
  }

  # correct settings
  set <- omopgenerics::settings(result)
  # remove columns that all are NA
  cols <- colnames(set) |>
    purrr::keep(\(x) any(!is.na(x)))
  set <- set |>
    dplyr::select(dplyr::all_of(cols))
  # replace NA for '-NA-'
  set <- set |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::where(is.character),
      .fns = \(x) dplyr::coalesce(x, "-NA-")
    ))

  # final result
  omopgenerics::newSummarisedResult(x = result, settings = set)
}


get_eudamed_device  <- function(udi) {

  results <- purrr::map_dfr(
    udi,
    ~ internal_get_eudamed_device(.x) |>
      dplyr::mutate(query_udi_di = .x)
  )

  results <- results |>
    dplyr::distinct()

  return(results)
}

internal_get_eudamed_device  <- function(udi_di) {

  url <- "https://api.datalake.sante.service.ec.europa.eu/eudamed/udi"

  response <- request(url) |>
    req_url_query(
      PRIMARY_DI = udi_di,
      format = "json",
      `api-version` = "v1.0"
    ) |>
    req_perform()

  resp_check_status(response)

  device <- resp_body_json(response)

  if(length(device$value) == 0) {
    result <- dplyr::tibble(
      "udi" = udi_di,
      "catalogue_number" = NA_character_,
      "basic_udi" = NA_character_,
      "device_name" = NA_character_,
      "nomenclature_code" = NA_character_,
      "manufacturer" = NA_character_)
  } else {
    result <- tibble("udi" = udi_di,
                     "catalogue_number" = device$value[[1]]$REFERENCE,
                     "basic_udi" = device$value[[1]]$BASIC_UDI,
                     "device_name" = device$value[[1]]$DEVICE_NAME,
                     "nomenclature_code" = device$value[[1]]$NOMENCLATURE_CODE,
                     "manufacturer" = device$value[[1]]$MF_NAME)
  }
  return(result)
}


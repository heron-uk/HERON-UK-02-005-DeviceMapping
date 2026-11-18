
# Check codeToRun inputs ----
omopgenerics::validateCdmArgument(cdm,
                                  requiredTables = c("person",
                                                     "observation_period",
                                                     "condition_occurrence",
                                                     "drug_exposure",
                                                     "concept"))
omopgenerics::assertNumeric(min_cell_count)

# Create a log file ----
createLogFile(logFile = here::here("results", "log_{date}_{time}"))
logMessage("LOG CREATED")

# Initialise list to store results as we go -----
results <- list()

# general summary ----
results[["snapshot"]] <- summariseOmopSnapshot(cdm)

logMessage("Table summary")
results[["tbl_summary"]] <- summariseClinicalRecords(cdm = cdm,
                                                     omopTableName = "device_exposure")

logMessage("Trend - overall")
results[["tbl_trend"]] <- summariseTrend(cdm = cdm,
                                         event = "device_exposure",
                                         interval = "years")

mapped <- c(TRUE, FALSE)
udi <-  c(TRUE, FALSE)
for(i in seq_along(mapped)){
  for(j in seq_along(udi)){

    working_cdm <- cdm
    working_mapped <- mapped[[i]]
    working_udi<- udi[[j]]

    cli::cli_inform("Trend - mapped {working_mapped} and udi {working_udi}")

    if(isTRUE(working_mapped)){
      # has mapping
      working_cdm$device_exposure <- working_cdm$device_exposure |>
        dplyr::filter(!is.na(device_concept_id) & device_concept_id != 0)
    } else {
      # not mapped
      working_cdm$device_exposure <- working_cdm$device_exposure |>
        dplyr::filter(is.na(device_concept_id) | device_concept_id == 0)
    }

    if(isTRUE(working_udi)){
      # has udi
      working_cdm$device_exposure <- working_cdm$device_exposure |>
        dplyr::filter(!is.na(unique_device_id) & unique_device_id != "")
    } else {
      # no udi
      working_cdm$device_exposure <- working_cdm$device_exposure |>
        dplyr::filter(is.na(unique_device_id) | unique_device_id == "")
    }

    results[[paste0("tbl_trend", working_mapped, working_udi)]] <- summariseTrend(cdm = working_cdm,
                                                                                  event = "device_exposure",
                                                                                  interval = "years")
    attr(results[[paste0("tbl_trend", working_mapped, working_udi)]],
         "settings") <- attr(results[[paste0("tbl_trend", working_mapped, working_udi)]],
                             "settings") |>
      mutate(mapped_to_standard = working_mapped,
             has_udi = working_udi)

  }}

# summary of udi -----
logMessage("udi summary")

counts <- cdm$device_exposure |>
  dplyr::select(
    "person_id",
    "device_concept_id",
    "unique_device_id",
    "device_source_concept_id"
  ) |>
  dplyr::mutate(
    device_concept_id = dplyr::coalesce(.data$device_concept_id, 0L),
    device_source_concept_id = dplyr::coalesce(.data$device_source_concept_id, 0L),
    unique_device_id = dplyr::coalesce(as.character(.data$unique_device_id), "unknown")
  ) |>
  dplyr::group_by(
    .data$device_concept_id,
    .data$unique_device_id,
    .data$device_source_concept_id
  ) |>
  dplyr::summarise(
    record_count = dplyr::n(),
    person_count = dplyr::n_distinct(.data$person_id),
    .groups = "drop"
  ) |>
  dplyr::left_join(
    cdm$concept |>
      dplyr::select(
        "device_concept_id" = "concept_id",
        "device_concept_id_name" = "concept_name"
      ),
    by = "device_concept_id"
  ) |>
  dplyr::left_join(
    cdm$concept |>
      dplyr::select(
        "device_source_concept_id" = "concept_id",
        "device_source_concept_id_name" = "concept_name",
        "device_source_concept_code" = "concept_code"
      ),
    by = "device_source_concept_id"
  ) |>
  dplyr::collect()

results[["udi_standard_source"]] <- counts |>
  dplyr::mutate(
    dplyr::across(dplyr::everything(), as.character)
  ) |>
  tidyr::pivot_longer(
    cols = c("record_count", "person_count"),
    names_to = "variable_name",
    values_to = "estimate_value"
  ) |>
  dplyr::mutate(
    variable_name = dplyr::if_else(
      .data$variable_name == "record_count",
      "number records",
      "number subjects"
    ),
    variable_level = NA_character_,
    estimate_name = "count",
    estimate_type = "integer",
    result_id = 1L,
    cdm_name = omopgenerics::cdmName(cdm),
    additional_name = "overall",
    additional_level = "overall"
  ) |>
  omopgenerics::uniteGroup(
    cols = c(
      "device_concept_id",
      "device_concept_id_name",
      "unique_device_id",
      "device_source_concept_id",
      "device_source_concept_id_name",
      "device_source_concept_code"
    )
  ) |>
  omopgenerics::uniteStrata() |>
  omopgenerics::newSummarisedResult(
    settings = dplyr::tibble(
      result_id = 1L,
      result_type = "summarise_table",
      package_name = "PatientProfiles",
      package_version = as.character(utils::packageVersion("PatientProfiles"))
    )
  )


# export results ------
logMessage("- export results")
results <- results |>
  vctrs::list_drop_empty() |>
  omopgenerics::bind()
exportSummarisedResult(results,
                       minCellCount = min_cell_count,
                       fileName = "results_{cdm_name}_{date}.csv",
                       path = here("results"))

cli::cli_alert_success("Study finished")

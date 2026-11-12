
# Check codeToRun inputs ----
omopgenerics::validateCdmArgument(cdm,
                                  requiredTables = c("person",
                                                     "observation_period",
                                                     "condition_occurrence",
                                                     "drug_exposure",
                                                     "concept"))
omopgenerics::assertNumeric(min_cell_count)

# Create a log file ----
createLogFile(logFile = tempfile(pattern = "log_{date}_{time}"))
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

results[["udi_standard_source"]] <- cdm$device_exposure |>
  mutate(year = clock::get_year(device_exposure_start_date),
         device_concept_id = dplyr::coalesce(device_concept_id, 0L),
         device_source_concept_id = dplyr::coalesce(device_source_concept_id, 0L),
         unique_device_id = dplyr::coalesce(as.character(unique_device_id), "unknown")) |>
  addConceptName("device_concept_id") |>
  addConceptName("device_source_concept_id") |>
  left_join(cdm$concept |>
              select("device_source_concept_id"="concept_id",
                     "device_source_concept_code" = "concept_code"),
            by = c("device_source_concept_id")) |>
  summariseResult(variables = character(),
                  group = c("device_concept_id",
                            "device_concept_id_name",
                            "unique_device_id",
                            "device_source_concept_id",
                            "device_source_concept_code",
                            "device_source_value"))

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

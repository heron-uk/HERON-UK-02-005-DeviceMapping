
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

results[["snapshot"]] <- summariseOmopSnapshot(cdm)

logMessage("Table summary")
results[["tbl_summary"]] <- summariseClinicalRecords(cdm = cdm,
                                                     omopTableName = "device_exposure")

logMessage("Trend - overall")
results[["tbl_trend"]] <- summariseTrend(cdm = cdm,
                                         event = "device_exposure",
                                         interval = "years")

mapped <- c(TRUE, FALSE)
uid <-  c(TRUE, FALSE)
for(i in seq_along(mapped)){
  for(j in seq_along(uid)){

    working_cdm <- cdm
    working_mapped <- mapped[[i]]
    working_uid<- uid[[j]]

    cli::cli_inform("Trend - mapped {working_mapped} and uid {working_uid}")

    if(isTRUE(working_mapped)){
      # has mapping
      working_cdm$device_exposure <- working_cdm$device_exposure |>
        dplyr::filter(!is.na(device_concept_id) & device_concept_id != 0)
    } else {
      # not mapped
      working_cdm$device_exposure <- working_cdm$device_exposure |>
        dplyr::filter(is.na(device_concept_id) | device_concept_id == 0)
    }

    if(isTRUE(working_uid)){
      # has uid
      working_cdm$device_exposure <- working_cdm$device_exposure |>
        dplyr::filter(!is.na(unique_device_id) & unique_device_id != "")
    } else {
      # no uid
      working_cdm$device_exposure <- working_cdm$device_exposure |>
        dplyr::filter(is.na(unique_device_id) | unique_device_id == "")
    }

    results[[paste0("tbl_trend", working_mapped, working_uid)]] <- summariseTrend(cdm = working_cdm,
                                                                                  event = "device_exposure",
                                                                                  interval = "years")
    attr(results[[paste0("tbl_trend", working_mapped, working_uid)]],
         "settings") <- attr(results[[paste0("tbl_trend", working_mapped, working_uid)]],
                             "settings") |>
      mutate(mapped_to_standard = working_mapped,
             has_uid = working_uid)

  }}

# summary of UID
logMessage("UID summary")
results[["uid_standard_source"]] <- cdm$device_exposure |>
  mutate(year = clock::get_year(device_exposure_start_date),
         # to ensure they don't get silently ignored
         device_concept_id = dplyr::coalesce(device_concept_id, 0L),
         device_source_concept_id = dplyr::coalesce(device_source_concept_id, 0L),
         unique_device_id = dplyr::coalesce(as.character(unique_device_id), "unknown")) |>
  addConceptName("device_concept_id") |>
  addConceptName("device_source_concept_id") |>
  summariseResult(variables = character(),
                  group = c("device_concept_id",
                            "device_concept_id_name",
                            "unique_device_id"))


logMessage("Characterise top 3 standard device concepts")
logMessage("- get top 3")
top_3_concepts <- cdm$device_exposure |>
  addConceptName("device_concept_id") |>
  group_by(device_concept_id,
           device_concept_id_name) |>
  tally() |>
  collect() |>
  filter(device_concept_id != "0") |>
  arrange(desc(n)) |>
  slice_head(n = 3)

cl <- top_3_concepts |>
  select(device_concept_id_name, device_concept_id) |>
  mutate(device_concept_id_name = paste0(device_concept_id, "_", device_concept_id_name)) |>
  tibble::deframe() |>
  as.list()
names(cl) <- omopgenerics::toSnakeCase(names(cl))
names(cl) <- stringr::str_trunc(names(cl), width = 22, ellipsis = "")

logMessage("- create cohorts")
cdm$top_3_device_concepts <- conceptCohort(cdm,
                                           conceptSet = cl,
                                           name = "top_3_device_concepts",
                                           exit = "event_start_date")
logMessage("- summarise characteristics")
results[["chars_top_3"]] <- cdm$top_3_device_concepts |>
  summariseCharacteristics()

logMessage("- summarise lsc")
results[["lsc_top_3"]] <- cdm$top_3_device_concepts |>
  summariseLargeScaleCharacteristics(window = list(c(0, 0),
                                                   c(-7, 7)),
                                     eventInWindow = c("procedure_occurrence",
                                                       "condition_occurrence",
                                                       "drug_exposure"),
                                     minimumFrequency = 0.005)

# logMessage("Procedure cohorts")
# cdm$proc <- conceptCohort(cdm,
#                           conceptSet = list(
#                             hip_replacement = c(4144432, 4203771, 4146785,
#                                                 44514773, 44514780, 44515507),
#                             pacemaker = c(4184306, 44511205, 44511204, 4180293,
#                                           4019139,4144921, 4180298,
#                                           44790501, 44790298, 44790432)),
#                           exit = "event_end_date",
#                           name = "proc")


logMessage("- export results")
results <- results |>
  vctrs::list_drop_empty() |>
  omopgenerics::bind()
exportSummarisedResult(results,
                       minCellCount = min_cell_count,
                       fileName = "results_{cdm_name}_{date}.csv",
                       path = here("results"))

cli::cli_alert_success("Study finished")

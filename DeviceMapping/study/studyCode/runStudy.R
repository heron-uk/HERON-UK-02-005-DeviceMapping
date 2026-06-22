
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

logMessage("Trend")
results[["tbl_trend"]] <- summariseTrend(cdm = cdm,
                            event = "device_exposure",
                            interval = "years")

logMessage("LSC summary")
cdm <- generateDenominatorCohortSet(cdm, "denom")
results[["lsc"]] <- summariseLargeScaleCharacteristics(cdm$denom,
                                          window = c(-Inf, Inf),
                                          eventInWindow = "device_exposure",
                                          includeSource = c(TRUE, FALSE),
                                          minimumFrequency = 0)

logMessage("UID summary")
results[["uid_standard"]] <- cdm$device_exposure |>
  addConceptName("device_concept_id") |>
  summariseResult(variables = c("unique_device_id"),
                  group = c("device_concept_id", "device_concept_id_name"),
                  includeOverallStrata = FALSE)
results[["uid_standard_source"]] <- cdm$device_exposure |>
  addConceptName("device_concept_id") |>
  addConceptName("device_source_concept_id") |>
  summariseResult(variables = c("unique_device_id"),
                  group = c("device_concept_id",
                            "device_concept_id_name",
                            "device_source_concept_id",
                            "device_source_concept_id_name",
                            "device_source_value"),
                  includeOverallStrata = FALSE)

logMessage("Characterise top 10 standard device concepts")
logMessage("- get top 10")
top_10_concepts <- cdm$device_exposure |>
  addConceptName("device_concept_id") |>
  group_by(device_concept_id,
           device_concept_id_name) |>
  tally() |>
  collect() |>
  arrange(desc(n)) |>
  slice_head(n = 10)

cl <- top_10_concepts |>
  select(device_concept_id_name, device_concept_id) |>
  mutate(device_concept_id_name = paste0(device_concept_id, "_", device_concept_id_name)) |>
  tibble::deframe() |>
  as.list()
names(cl) <- omopgenerics::toSnakeCase(names(cl))
names(cl) <- stringr::str_trunc(names(cl), width = 22, ellipsis = "")

logMessage("- create cohorts")
cdm$top_ten_device_concepts <- conceptCohort(cdm,
                                             conceptSet = cl,
                                             name = "top_ten_device_concepts",
                                             exit = "event_start_date")
logMessage("- summarise characteristics")
chars_top_ten <- cdm$top_ten_device_concepts |>
  summariseCharacteristics()

logMessage("- summarise lsc")
lsc_top_ten <- cdm$top_ten_device_concepts |>
  summariseLargeScaleCharacteristics(window = list(c(0, 0),
                                                   c(-7, 7)),
                                     eventInWindow = c("condition_occurrence",
                                                       "procedure_occurrence",
                                                       "drug_exposure"),
                                     minimumFrequency = 0.005)


logMessage("- export results")
results <- results |>
  vctrs::list_drop_empty() |>
  omopgenerics::bind()
exportSummarisedResult(results,
                       minCellCount = min_cell_count,
                       fileName = "results_{cdm_name}_{date}.csv",
                       path = here("results"))

cli::cli_alert_success("Study finished")


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

if(cdmName(cdm) == "LTH OMOP Bronze"){
  study_period <- as.Date(c("2022-01-01", NA))
} else {
  study_period <- as.Date(c(NA,NA))
}

logMessage("STARTING ANALYSES")
source(here("analyses", "1-DeviceExposureSummary.R"))
source(here("analyses", "2-SummaryOfUDI.R"))
source(here("analyses", "3-CaseStudies.R"))

logMessage("- export results")
results <- results |>
  vctrs::list_drop_empty() |>
  omopgenerics::bind()
exportSummarisedResult(results,
                       minCellCount = min_cell_count,
                       fileName = "results_{cdm_name}_{date}.csv",
                       path = here("results"))

cli::cli_alert_success("Study finished")

# create logger ----
resultsFolder <- here("results")
if(!dir.exists(resultsFolder)){
  dir.create(resultsFolder)
}

createLogFile(logFile = tempfile(pattern = "log_{date}_{time}"))
logMessage("LOG CREATED")


# cohorts ----
if("hip" %in% procedures_to_run){
  logMessage("GETTING HIP COHORTS")
  source(here("hip", "cohorts", "instantiateCohorts.R"))
}
if("lens" %in% procedures_to_run){
  logMessage("GETTING LENS COHORTS")
  source(here("lens", "cohorts", "instantiateCohorts.R"))
}
if("pacemaker" %in% procedures_to_run){
  logMessage("GETTING PACEMAKER COHORTS")
  source(here("pacemaker", "cohorts", "instantiateCohorts.R"))
}

logMessage("COMBINE INTO SINGLE COHORT TABLE")
cdm <- do.call(
  what = bind,
  args = c(
    lapply(procedures_to_run, function(x) cdm[[x]]),
    list(name = "study_cohorts")
  )
)

# run ----
logMessage("Running PhenotypeDiagnostics")
diagnostics <- phenotypeDiagnostics(cdm$study_cohorts,
                                    populationDiagnostics = list(cohortId = "asthma"),
                                    cohortDiagnostics = list(compareCohorts = TRUE,
                                                             cohortId = "asthma"),
                                    codelistDiagnostics = list(measurementDiagnostics = TRUE))

exportSummarisedResult(diagnostics,
                       minCellCount = minCellCount,
                       fileName = "phenotyper_results_{cdm_name}_{date}.csv",
                       path = resultsFolder)
logMessage("Finished")

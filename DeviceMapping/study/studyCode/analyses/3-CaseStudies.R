# case studies -----
logMessage("Characterise top selected standard device concepts")
device_concepts <- c(
  # opthamology - lens
  45758993, # Posterior-chamber intraocular lens, pseudophakic
  45758380, # Anterior-chamber intraocular lens, pseudophakic
  # cardiology - pacemaker
  45772840, # Implantable cardiac pacemaker
  2615795, # Pacemaker, dual chamber, rate-responsive (implantable)
  2615796, # Pacemaker, single chamber, rate-responsive (implantable)
  # cardiology - icd
  37164898, # Subcutaneous implantable cardioverter defibrillator
  45771731, # Single-chamber implantable defibrillator
  45771922, # Dual-chamber implantable defibrillator
  # orthopedic
  45761725, # Ceramic femoral head prosthesis
  45758713 # Metallic femoral head prosthesis
)

dc <- device_concepts |>
  as.list()
names(dc) <- paste0("concept_", dc)

logMessage("- create device cohorts")
cdm$dc <- conceptCohort(cdm,
                        conceptSet = dc,
                        name = "dc",
                        exit = "event_start_date")
cdm$dc <- cdm$dc |>
  requireInDateRange(dateRange = study_period)

# add unique identifiers - may have more than one
cdm$udi <- cdm$dc |>
  dplyr::inner_join(
    cdm$device_exposure |>
      dplyr::filter(device_concept_id %in%
                      !!dc |> unlist()) |>
      dplyr::select("subject_id"="person_id",
                    "cohort_start_date" = "device_exposure_start_date",
                    "unique_device_id") |>
      dplyr::mutate(unique_device_id = if_else(is.na(unique_device_id), "unknown",
                                               unique_device_id)) |>
      dplyr::mutate(unique_device_id = paste0("udi_", unique_device_id)),
    by = c("subject_id",
           "cohort_start_date")) |>
  dplyr::select("subject_id", "cohort_start_date", "unique_device_id") |>
  dplyr::compute(name = "udi")

cdm$udi <- cdm$udi |>
  dplyr::mutate(flag = "Yes") |>
  tidyr::pivot_wider(names_from = "unique_device_id",
                     values_from = "flag") |>
  dplyr::compute(name = "udi")

newCols <- colnames(cdm$udi)[!colnames(cdm$udi) %in%
                               c("subject_id", "cohort_start_date")]

cdm$dc <- cdm$dc |>
  dplyr::left_join(cdm$udi,
                   by = c("subject_id",
                          "cohort_start_date"))
cdm$dc <- cdm$dc |>
  omopgenerics::newCohortTable()


logMessage("- create table one cohorts")
table_1_codes <- importCodelist(here::here("codelists"), type = "csv")
cdm$table_1_cohorts <- conceptCohort(cdm,
                                     conceptSet = table_1_codes,
                                     name = "table_1_cohorts",
                                     exit = "event_start_date")

logMessage("- summarise attrition")
results[["attr_dc"]] <- cdm$dc |>
  summariseCohortAttrition()
results[["attr_t1"]] <- cdm$table_1_cohorts |>
  summariseCohortAttrition()

logMessage("- summarise code use")
results[["code_use_dc"]] <- CodelistGenerator::summariseCohortCodeUse(cdm, "dc")
results[["code_use_t1"]] <- CodelistGenerator::summariseCohortCodeUse(cdm, "table_1_cohorts")

logMessage("- summarise characteristics")
results[["chars_dc"]] <- cdm$dc |>
  summariseCharacteristics(
    cohortIntersectFlag = list(
      targetCohortTable = "table_1_cohorts",
      window = c(-7, 7)
    ),
    otherVariables = newCols)

logMessage("- summarise lsc")
results[["lsc_dc"]] <- cdm$dc |>
  summariseLargeScaleCharacteristics(window = list(c(-7, 7),
                                                   c(-Inf, 7)),
                                     eventInWindow = c("procedure_occurrence",
                                                       "condition_occurrence",
                                                       "drug_exposure",
                                                       "observation"),
                                     minimumFrequency = 0.005)

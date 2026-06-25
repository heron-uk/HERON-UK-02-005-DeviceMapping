
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


logMessage("Characterise top selected standard device concepts")
device_concepts <- c(
# opthamology - lens
45758993, # Posterior-chamber intraocular lens, pseudophakic
45758380, # Anterior-chamber intraocular lens, pseudophakic
# cardiology - valve replacement
3661561, # Aortic valve bioprosthesis
3661562, # Mitral valve bioprosthesis
# cardiology - pacemaker
4231009, # Cardiac pacemaker electrode
45772840, # Implantable cardiac pacemaker
2615795, # Pacemaker, dual chamber, rate-responsive (implantable)
2615796, # Pacemaker, single chamber, rate-responsive (implantable)
# repiratory
45762420, # Endobronchial valve
# orthopedic
46273109, # Femoral stem centralizer
45761725, # Ceramic femoral head prosthesis
45761793, # Acetabular shell
45760907, # Uncoated knee tibia prosthesis, polyethene
45763037, # Bipolar femoral head prosthesis
45760762, # Uncoated hip femur prosthesis, modular
45761165, # Uncoated knee tibia prosthesis, metallic
45758713, # Metallic femoral head prosthesis
45758465, # Polyethylene patella prosthesis
45772469, # Coated hip femur prosthesis, modular
45760486, # Uncoated femoral stem prosthesis, modular
45768074, # Knee tibia prosthesis
45761776, # Coated femoral stem prosthesis, modular
45771801, # Uncoated knee femur prosthesis
# mesh
45768044, # Surgical mesh
4223318, # Mesh
45765017 # Pelvic organ prolapse surgical mesh, composite
)

dc <- device_concepts |>
  as.list()
names(dc) <- paste0("concept_", dc)

logMessage("- create cohorts")
cdm$dc <- conceptCohort(cdm,
                        conceptSet = dc,
                        name = "dc",
                        exit = "event_start_date")

logMessage("- summarise characteristics")
results[["chars_dc"]] <- cdm$dc |>
  summariseCharacteristics()

logMessage("- summarise lsc")
results[["chars_dc"]] <- cdm$dc |>
  summariseLargeScaleCharacteristics(window = list(c(-7, 7)),
                                     eventInWindow = c("procedure_occurrence",
                                                       "condition_occurrence",
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

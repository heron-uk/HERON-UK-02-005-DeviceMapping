# summary of udi -----
logMessage("udi summary")

if(!is.na(study_period[[1]])) {
  cdm$device_exposure <- cdm$device_exposure |>
    filter(drug_exposure_start_date >= study_period[[1]]) |>
    compute(temporary = FALSE, name = "device_exposure")
}

if(!is.na(study_period[[2]])) {
  cdm$device_exposure <- cdm$device_exposure |>
    filter(drug_exposure_end_date <= study_period[[2]]) |>
    compute(temporary = FALSE, name = "device_exposure")
}

results[["udi_standard_source"]] <- cdm$device_exposure |>
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

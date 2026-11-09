
# diagnoses/ meds
t1_diabetes_codes <- CodelistGenerator::getDescendants(cdm, 201254)$concept_id
t2_diabetes_codes <- CodelistGenerator::getDescendants(cdm, 201826)$concept_id
# insulin_codes <- CodelistGenerator::getCandidateCodes(cdm, "insulin", domains = "drug")$concept_id

# device snomed
cgm_codes <- list(
  cgm_dexcom_g6_codes = c(891070, 882115, 888630), # Dexcom G6 Sensor
  cgm_dexcom_g7_codes = c(885453, 889729, 891653), # Dexcom G7 Sensor
  cgm_dexcom_one_codes = c(36964253, 36924428, 36944703), # Dexcom One Sensor
  cgm_dexcom_one_plus_codes = c(886971, 885073), # Dexcom One+
  cgm_freestyle_libre_2_plus_codes = c(888893, 896954, 35402057), # FreeStyle Libre 2 Plus Sensor
  cgm_freestyle_libre_2_codes = c(35889967, 35891819), # FreeStyle Libre 2 Sensor
  cgm_freestyle_libre_3_plus_codes = c(837419, 879292), # FreeStyle Libre 3 Plus Sensor
  cgm_freestyle_libre_3_codes = c(880079, 897543), # FreeStyle Libre 3 Sensor
  cgm_freestyle_libre_plus_codes = c(40677414, 42524374, 40675740) #FreeStyle Libre Sensor
) |>
  newCodelist()
cgm_codes <- cgm_codes |>
  unionCodelists("cgm",
                 keepOriginal = TRUE)


# export codelists
cgm_codes |>
  exportCodelist(here::here("cgm", "cohorts","codelists"), type = "csv")



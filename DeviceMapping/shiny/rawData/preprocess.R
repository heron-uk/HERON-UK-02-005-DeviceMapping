# shiny is prepared to work with this resultList:
resultList <- list(
  summarise_omop_snapshot = list(result_type = "summarise_omop_snapshot"),
  summarise_clinical_records = list(result_type = "summarise_clinical_records"),
  summarise_trend = list(result_type = "summarise_trend"),
  summarise_concept_id_counts = list(result_type = "summarise_concept_id_counts"),
  summarise_characteristics = list(result_type = "summarise_characteristics"),
  summarise_large_scale_characteristics = list(result_type = "summarise_large_scale_characteristics"),
  summarise_log_file = list(result_type = "summarise_log_file"),
  summarise_table = list(result_type = "summarise_table")
)

source(file.path(getwd(), "functions.R"))

result <- omopgenerics::importSummarisedResult(file.path(getwd(), "rawData"))
data <- prepareResult(result, resultList)
values <- getValues(result, resultList)

# edit choices and values of interest
choices <- values
selected <- getSelected(values)

save(data, choices, selected, values, file = file.path(getwd(), "data", "studyData.RData"))

rm(result, values, choices, selected, resultList, data)

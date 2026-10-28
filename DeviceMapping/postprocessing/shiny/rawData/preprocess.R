# shiny is prepared to work with this resultList:
resultList <- list(
  summarise_omop_snapshot = list(result_type = "summarise_omop_snapshot"),
  summarise_clinical_records = list(result_type = "summarise_clinical_records"),
  summarise_trend = list(result_type = "summarise_trend"),
  cohort_code_use = list(result_type = "cohort_code_use"),
  summarise_cohort_attrition = list(result_type = "summarise_cohort_attrition"),
  summarise_characteristics = list(result_type = "summarise_characteristics"),
  summarise_large_scale_characteristics = list(result_type = "summarise_large_scale_characteristics"),
  summarise_log_file = list(result_type = "summarise_log_file"),
  summarise_table = list(result_type = "summarise_table")
)

source(file.path(getwd(), "functions.R"))

result <- omopgenerics::importSummarisedResult(here("..","results", "results_v2.csv"))
result <- result |>
  mutate("cdm_name" = gsub("BartsHealthOMOP", "Barts Health", cdm_name),
         "cdm_name" = gsub("IDRIL_1", "Lancashire", cdm_name)) 

attr(result, "settings") <- attr(result, "settings") |> 
  mutate(trend_type = case_when(
    has_udi == "TRUE" & mapped_to_standard == "TRUE" ~ "Standard concept,\nwith unique identifier",
    has_udi == "TRUE" & mapped_to_standard == "FALSE" ~ "No standard concept,\nwithout unique identifier",
    has_udi == "FALSE" & mapped_to_standard == "TRUE" ~ "Standard concept,\nwithout unique identifier",
    has_udi == "FALSE" & mapped_to_standard == "FALSE" ~ "No standard concept,\nwith unique identifier",
    .default = "All records"
  ))

data <- prepareResult(result, resultList)
values <- getValues(result, resultList)

values$summarise_characteristics_cohort_name <- c("Pacemaker", "Femoral head", "Intraocular lens")

for (nm in names(values)[grepl("summarise_characteristics_", names(values))]) {
  new_nm <- sub("summarise_characteristics_", "summarise_device_characteristics_", nm)
  values[[new_nm]] <- values[[nm]]
}

# edit choices and values of interest
choices <- values
selected <- getSelected(values)

save(data, choices, selected, values, file = file.path(getwd(), "data", "studyData.RData"))

rm(result, values, choices, selected, resultList, data)

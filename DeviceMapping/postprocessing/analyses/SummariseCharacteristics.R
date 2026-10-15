library(omopgenerics)
library(here)
library(dplyr)
source(here("functions.R"))

# stratify result into a list
result <- importSummarisedResult(path = here("results", "results.csv"))

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

data <- prepareResult(result = results, resultList = resultList)

res_all <- data$summarise_characteristics |>
  splitAdditional() |>
  filter(device_name != "overall") |>
  mutate(estimate_value = if_else(grepl("-", estimate_value), 0L, as.numeric(estimate_value))) |>
  group_by(result_id, cdm_name, group_name, group_level,
           strata_name, strata_level, device_name, estimate_name, estimate_type,
           manufacturer, catalogue_number) |>
  summarise(estimate_value = sum(estimate_value, na.rm = TRUE), .groups = "drop_last") |>
  ungroup() |>
  distinct() |>
  filter(estimate_value != 0) |>
  mutate(variable_name = device_name,
         variable_level = NA_character_) |>
  uniteAdditional(c("device_name", "manufacturer", "catalogue_number")) |>
  mutate("result_id" = 1L) |>
  newSummarisedResult(settings = tibble(
    "result_id" = 1L,
    "result_type" = "summarise_characteristics",
    "package_name" = "CohortCharacteristics",
    "package_version" = "1.1.1",
    "min_cell_count" = 5
  ))

res_dn_man <- data$summarise_characteristics |>
  splitAdditional() |>
  filter(device_name != "overall") |>
  mutate(estimate_value = if_else(grepl("-", estimate_value), 0L, as.numeric(estimate_value))) |>
  group_by(result_id, cdm_name, group_name, group_level,
           strata_name, strata_level, device_name, estimate_name, estimate_type, manufacturer) |>
  summarise(estimate_value = sum(estimate_value, na.rm = TRUE), .groups = "drop_last") |>
  ungroup() |>
  filter(estimate_value != 0) |>
  mutate(variable_name = device_name,
         variable_level = NA_character_) |>
  uniteAdditional(c("device_name", "manufacturer")) |>
  mutate("result_id" = 1L) |>
  newSummarisedResult(settings = tibble(
    "result_id" = 1L,
    "result_type" = "summarise_characteristics",
    "package_name" = "CohortCharacteristics",
    "package_version" = "1.1.1",
    "min_cell_count" = 5
  ))

res_dn <- data$summarise_characteristics |>
  splitAdditional() |>
  filter(device_name != "overall") |>
  mutate(estimate_value = if_else(grepl("-", estimate_value), 0L, as.numeric(estimate_value))) |>
  group_by(result_id, cdm_name, group_name, group_level,
           strata_name, strata_level, device_name, estimate_name, estimate_type) |>
  summarise(estimate_value = sum(estimate_value, na.rm = TRUE), .groups = "drop_last") |>
  ungroup() |>
  mutate(variable_name = device_name,
         variable_level = NA_character_) |>
  uniteAdditional(c("device_name")) |>
  mutate("result_id" = 1L) |>
  filter(estimate_value != 0) |>
  newSummarisedResult(settings = tibble(
    "result_id" = 1L,
    "result_type" = "summarise_characteristics",
    "package_name" = "CohortCharacteristics",
    "package_version" = "1.1.1",
    "min_cell_count" = 5
  ))

data$summarise_characteristics <- bind(data$summarise_characteristics |>
                                         filterAdditional(device_name == "overall"),
                                       res_all,
                                       res_dn_man,
                                       res_dn)
results <- bind(data)

exportSummarisedResult(results, fileName = "results_v2.csv", path = here("results"), minCellCount = 5)

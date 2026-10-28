library(httr2)
library(httr)
library(jsonlite)
library(omopgenerics)
library(here)
library(purrr)
library(dplyr)
library(tibble)
library(AccessGUDID)
library(readr)
source(here("functions.R"))

map_devices <- FALSE

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

results <- importSummarisedResult(path = here("data"))

results <- results |>
  mutate(group_level = case_when(
    group_level == "concept_2615795" ~ "Dual chamber pacemaker",
    group_level == "concept_2615796" ~ "Single chamber pacemaker",
    group_level == "concept_45758380" ~ "Anterior intraocular lens",
    group_level == "concept_45758713" ~ "Metallic femoral head",
    group_level == "concept_45758993" ~ "Posterior intraocular lens",
    group_level == "concept_45761725" ~ "Ceramic femoral head",
    group_level == "concept_45772840" ~ "Implantable cardiac pacemaker",
    .default = group_level))
data <- prepareResult(result = results, resultList = resultList)

data$summarise_characteristics <- bind(
  data$summarise_characteristics |>
    filter(!grepl("Udi", variable_name)),
  data$summarise_characteristics |>
    filter(grepl("Udi", variable_name)) |>
    splitAdditional() |>
    mutate("udi" = gsub("Udi ", "", variable_name)) |>
    uniteAdditional(cols = "udi"))

# Get list of UDIs present in the data ----
if(isTRUE(map_devices)) {
  listOfUDIs <- data$summarise_characteristics |>
    filter(!additional_level %in% c("overall", "unknown")) |>
    select(additional_level) |>
    distinct() |>
    pull("additional_level")

  # Get UDIs via EUDAMED
  udi_tibble_eudamed  <- get_eudamed_device(listOfUDIs)

  # Get UDIs via ACCESGUID
  dir_snomed <- downloadSnomedFiles(apiKey = Sys.getenv("api_key"),
                                    directory = here("AccessGUDID"))

  dir_files <- downloadFiles(directory = here("AccessGUDID"))
  udi_tibble_accessguid <- getSnomed(snomedFilesDirectory = dir_snomed,
                                     filesDirectory = dir_files,
                                     deviceDI = listOfUDIs)

  # Post processing
  udi_tibble_eudamed <- udi_tibble_eudamed |>
    select("udi",
           "catalogue_number_eudamed" = "catalogue_number",
           "device_name_eudamed" = "device_name",
           "manufacturer_eudamed" = "manufacturer")

  udi_tibble_accessguid <- udi_tibble_accessguid |>
    mutate("brand_name" = if_else(brand_name == "-", device_description, brand_name)) |>
    select("udi" = "device_di",
           "catalogue_number_accessguid" = "catalogue_number",
           "device_name_accessguid" = "brand_name",
           "manufacturer_accessguid" = "company_name") |>
    mutate("catalogue_number_accessguid" = gsub("-", NA_character_, catalogue_number_accessguid)) |>
    mutate("manufacturer_accessguid" = gsub("-", NA_character_, manufacturer_accessguid))

  udi_tibble <- udi_tibble_eudamed |>
    inner_join(udi_tibble_accessguid,
               by = "udi") |>
    mutate("source" = case_when(
      is.na(device_name_eudamed) & device_name_accessguid == "No device identified" ~ "-",
      is.na(device_name_eudamed) & device_name_accessguid != "No device identified" ~ "FDA",
      .default = "EUDAMED"
    )) |>
    mutate("device_name" = if_else(!is.na(device_name_eudamed), device_name_eudamed, device_name_accessguid),
           "catalogue_number" = if_else(!is.na(catalogue_number_eudamed), catalogue_number_eudamed, catalogue_number_accessguid),
           "manufacturer" = if_else(!is.na(manufacturer_eudamed), manufacturer_eudamed, manufacturer_accessguid))

  udi_tibble |> write_csv(here("results", "mapped_udis.csv"))
}

udi_tibble <- read_csv(here("results", "mapped_udis.csv"))

data$summarise_characteristics <- data$summarise_characteristics |>
  mutate("variable_name" = if_else(grepl("Udi", variable_name), "Device characteristics", variable_name)) |>
  splitAdditional() |>
  left_join(udi_tibble |>
              select("source", "manufacturer", "device_name", "catalogue_number", "udi"), by = "udi") |>
  mutate(across(c("source", "manufacturer", "device_name", "catalogue_number"), ~coalesce(., "overall"))) |>
  uniteAdditional(c("source", "manufacturer", "device_name", "catalogue_number", "udi"))

results <- bind(data)

exportSummarisedResult(results, fileName = "results.csv", path = here("results"))

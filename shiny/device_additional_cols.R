library(DBI)
library(duckdb)
library(here)
library(purrr)
library(tools)
library(dplyr)
library(dbplyr)
library(omopgenerics)
library(ggplot2)
library(visOmopResults)
library(tidyr)
library(scales)
library(stringr)
library(patchwork)
library(httr2)

res <- importSummarisedResult(here::here("rawData"))
omop_dmd_groups <- readr::read_csv(here::here("omop_dmd","omop_dmd_groups.csv"))
chat <- ellmer::chat_google_gemini(model =  "gemini-3.5-flash-lite",
                                   params = params(temperature = 0))

# devices in the network ------
devices <- res |> 
  filterSettings(result_type == "summarise_table") |> 
  tidy() |> 
  filter(variable_name == "number records") |> 
  group_by(cdm_name,
           device_concept_id,
           device_concept_id_name,
           unique_device_id) |> 
  select("cdm_name",
         "device_concept_id",
         "device_concept_id_name",
         "unique_device_id") |> 
  unique()
  
# add dmd vmp name -----  
devices <- devices |> 
  left_join(omop_dmd_groups |> 
              mutate(concept_id = as.character(concept_id)) |> 
              select("concept_id",
                     "dmd_vmp_name" = "vmp_name"),
            by = c("device_concept_id" = "concept_id"))

# Add FDA GUDID UDI info -----
source("functions_gudid.R")
# build_gudid_duckdb(
#   here::here("AccessGUDID_Delimited_Full_Release_20260202"),
#   overwrite = TRUE
# )
db <- DBI::dbConnect(duckdb(dbdir = here::here("gudid.duckdb")))
gudid <- gudidFromCon(db)

devices <- devices |> 
  left_join(gudid$device |> 
              select("unique_device_id" = "PrimaryDI",
                     "gudid_brand_name" = "brandName",
                     "gudid_company_name" = "companyName",
                     "gudid_device_description" = "deviceDescription"),
            by = c("unique_device_id"),
            copy = TRUE)

# Add eudamed UDI info ---- 
source("functions_eudamed.R")
# get_eudamed_device("00889024263567") # test udi that should work
# eudamed_devices <- get_eudamed_device(udi = udi_counts$unique_device_id |> unique())

# Add llm classifications -----
source("classify_medical_devices.R")

## 1) categorise dmd codes ------
# will use the vmp code to reduce number and just use the vmp name
# filtered to only those in use
devices <- devices |>
  unite(
    col = "device_name",
    device_concept_id_name,
    dmd_vmp_name,
    gudid_brand_name,
    gudid_company_name,
    gudid_device_description,
    sep = "; ",
    na.rm = TRUE,
    remove = FALSE
  )

devices <- classify_medical_devices(chat = chat, 
                                    devices) 

# save -----
readr::write_csv(devices, file = here::here("devices.csv"))

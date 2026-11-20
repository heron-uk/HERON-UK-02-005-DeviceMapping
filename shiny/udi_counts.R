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

# udi in network ------
udi_counts <- res |> 
  filterSettings(result_type == "summarise_table") |> 
  tidy() |> 
  filter(variable_name == "number records") |> 
  filter(unique_device_id != "unknown") |> 
  group_by(unique_device_id) |> 
  summarise(
    # device_concept_id_name = paste(unique(device_concept_id_name), collapse = "; ") ,
    # cdm_name = paste(unique(cdm_name), collapse = "; "),
    .groups = "drop"
  ) |> 
  distinct()



# FDA GUDID -----
source("functions_gudid.R")
# build_gudid_duckdb(
#   here::here("AccessGUDID_Delimited_Full_Release_20260202"),
#   overwrite = TRUE
# )
db <- DBI::dbConnect(duckdb(dbdir = here::here("gudid.duckdb")))
gudid <- gudidFromCon(db)

udi_counts <- udi_counts |> 
  left_join(gudid$device |> 
              select("unique_device_id" = "PrimaryDI",
                     "gudid_brand_name" = "brandName",
                     "gudid_company_name" = "companyName",
                     "gudid_device_description" = "deviceDescription") |> 
              mutate(in_gudid = 1L),
            by = c("unique_device_id"),
            copy = TRUE)
udi_counts <- udi_counts |> 
  mutate(in_gudid = if_else(is.na(in_gudid),
                            0, in_gudid))

# eudamed ---- 
source("functions_eudamed.R")
# get_eudamed_device("00889024263567") # test udi that should work
# eudamed_devices <- get_eudamed_device(udi = udi_counts$unique_device_id |> unique())

# save -----
udi_counts <- udi_counts |> 
  filter(in_gudid == 1)
readr::write_csv(udi_counts, file = here::here("udi_details", "udi_counts.csv"))

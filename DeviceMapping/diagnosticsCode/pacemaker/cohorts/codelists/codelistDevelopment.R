
# diagnoses
arrhythmia_codes <- CodelistGenerator::getDescendants(cdm, 44784217)$concept_id

# procedure codes
pacemaker_implantation_codes <- CodelistGenerator::getDescendants(cdm, c(4051938))$concept_id

# device snomed
pacemaker_codes <- c(45772840, # Implantable cardiac pacemaker
                     45765309, # Intracardiac pacemaker
                     2615795, # Pacemaker, dual chamber, rate-responsive (implantable)
                     45767340, # Dual chamber rate responsive implantable cardiac pacemaker
                     2615796, # Pacemaker, single chamber, rate-responsive (implantable)
                     45767379 # Single chamber rate responsive implantable cardiac pacemaker
)
pacemaker_dual_codes <- c(2615795, 45767340)
pacemaker_single_codes <- c(2615796, 45767379)



# udi
st_jude_dr_assurity_udi_codes <- 05414734509589 |> as.character()
st_jude_sr_assurity_udi_codes <-05414734509572 |> as.character()
st_jude_aveir_udi_codes <- 05415067034472 |> as.character()
biotronik_amvia_sky_udi_codes <- 04035479174026 |> as.character()
medronic_dr_azure_surescan_udi_codes <- 00763000316693 |> as.character()
medronic_sensia_udi_codes <- 00613994177957 |> as.character()
medronic_sr_azure_surescan_udi_codes <- 00763000316716 |> as.character()

# export codelists
list(arrhythmia = arrhythmia_codes,
     pacemaker_implantation = pacemaker_implantation_codes,
     pacemaker = pacemaker_codes,
     pacemaker_dual = pacemaker_dual_codes,
     pacemaker_single = pacemaker_single_codes) |>
  newCodelist() |>
  exportCodelist(here::here("pacemaker", "cohorts","codelists"), type = "csv")

# export udi
readr::write_csv(tibble(udi = st_jude_dr_assurity_udi_codes),
                 here::here("pacemaker", "cohorts","udi", "st_jude_dr_assurity_udi.csv"))
readr::write_csv(tibble(udi = st_jude_sr_assurity_udi_codes),
                 here::here("pacemaker", "cohorts","udi", "st_jude_sr_assurity_udi.csv"))
readr::write_csv(tibble(udi = st_jude_aveir_udi_codes),
                 here::here("pacemaker", "cohorts","udi", "st_jude_aveir_udi.csv"))
readr::write_csv(tibble(udi = biotronik_amvia_sky_udi_codes),
                 here::here("pacemaker", "cohorts","udi", "biotronik_amvia_sky_udi.csv"))
readr::write_csv(tibble(udi = medronic_dr_azure_surescan_udi_codes),
                 here::here("pacemaker", "cohorts","udi", "medronic_dr_azure_surescan_udi.csv"))
readr::write_csv(tibble(udi = medronic_sensia_udi_codes),
                 here::here("pacemaker", "cohorts","udi", "medronic_sensia_udi.csv"))
readr::write_csv(tibble(udi = medronic_sr_azure_surescan_udi_codes),
                 here::here("pacemaker", "cohorts","udi", "medronic_sr_azure_surescan_udi.csv"))


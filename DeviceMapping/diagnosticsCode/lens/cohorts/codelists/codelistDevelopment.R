
# diagnoses
cataract_codes <-  CodelistGenerator::getDescendants(cdm, 375545)$concept_id

# procedure codes
lens_implantation_codes <- CodelistGenerator::getDescendants(cdm, 4288992)$concept_id

# device snomed
lens_codes <- c(45758993, # Posterior-chamber intraocular lens, pseudophakic
                45758380 # Anterior-chamber intraocular lens, pseudophakic
)
lens_posterior_codes <- 45758993
lens_anterior_codes <- 45758380

# udi
bl_sofport_iol_udi_codes <- c(10757770474346,
                              10757770474339,
                              10757770474377,
                              10757770474353,
                              10757770474315) |> as.character()

bl_akreos_micro_udi_codes <- c(10757770500854,
                               10757770500830,
                               10757770500878,
                               10757770500861,
                               10757770500847,
                               10757770500823,
                               10757770500816,
                               10757770500885,
                               10757770500892,
                               10757770500908,
                               10757770500809,
                               10757770500915,
                               10757770500786,
                               10757770500779,
                               10757770500793,
                               10757770500939,
                               10757770500922,
                               10757770500755,
                               10757770500748,
                               10757770500946,
                               10757770500717,
                               10757770500953,
                               10757770500700,
                               10757770500731,
                               10757770500762,
                               10757770500601,
                               10757770500618,
                               10757770500724,
                               10757770500649,
                               10757770500656,
                               10757770500687,
                               10757770500694,
                               10757770500984,
                               10757770501011,
                               10757770500670,
                               10757770500977) |> as.character()

jnj_tecnis_iol_codes <- c(05050474529052) |> as.character()

alcon_acrsysof_udi_codes <-c(00380652552240,
                             00380650049025) |> as.character()



# export codelists
list(cataract = cataract_codes,
     lens_implantation = lens_implantation_codes,
     lens = lens_codes,
     lens_posterior = lens_posterior_codes,
     lens_anterior = lens_anterior_codes) |>
  newCodelist() |>
  exportCodelist(here::here("lens", "cohorts","codelists"), type = "csv")

# export udi
readr::write_csv(tibble(udi = bl_sofport_iol_udi_codes),
                 here::here("lens", "cohorts","udi", "bl_sofport_iol_udi_codes.csv"))
readr::write_csv(tibble(udi = bl_akreos_micro_udi_codes),
                 here::here("lens", "cohorts","udi", "bl_akreos_micro_udi.csv"))
readr::write_csv(tibble(udi = jnj_tecnis_iol_codes),
                 here::here("lens", "cohorts","udi", "jnj_tecnis_iol_udi.csv"))
readr::write_csv(tibble(udi = alcon_acrsysof_udi_codes),
                 here::here("lens", "cohorts","udi", "alcon_acrsysof_udi.csv"))



# diagnoses
hip_fx_codes <- CodelistGenerator::getDescendants(cdm, 433856) |>
  pull(concept_id)
hip_oa_codes <- CodelistGenerator::getDescendants(cdm, 4079749) |>
  pull(concept_id)

# procedure codes
hemi_codes <- c(4290407, # maps from W46.1 (Primary prosthetic replacement of head of femur using cement)
                4077078, # maps from W47.1 (Primary prosthetic replacement of head of femur not using cement)
                44514853 # W48.1 (Primary prosthetic replacement of head of femur NEC)
)
hemi_cemented_codes <- 4290407
hemi_uncemented_codes <- 4077078

hemi_revision_codes <- c(4079261, # Revision cemented hemiarthroplasty of hip
                         4079263) # Revision of prosthetic replacement of head of femur not using cement
hemi_cemented_revision_codes <- 4079261
hemi_uncemented_revision_codes <- 4079263

hip_replacement_codes <- CodelistGenerator::getDescendants(cdm, 4203771) |>
  filter(str_detect(concept_name, regex("revision", ignore_case = TRUE), negate = TRUE)) |>
  filter(str_detect(concept_name, regex("conversion of previous hip surgery", ignore_case = TRUE), negate = TRUE)) |>
  pull(concept_id)
hip_revision_codes <- CodelistGenerator::getDescendants(cdm, 4090926) |>
  pull(concept_id)

# device snomed
femoral_head_codes <- c(45763037, #Bipolar femoral head prosthesis
                        45761725, #Ceramic femoral head prosthesis
                        45758713 # Metallic femoral head prosthesis
)
femoral_head_bipolar_codes <- 45763037
femoral_head_ceramic_codes <- 45761725
femoral_head_metallic_codes <- 45758713

femur_prosthesis_codes <- c(45760762, # Uncoated hip femur prosthesis, modular
                            45772469 # Coated hip femur prosthesis, modular
)
femur_prosthesis_uncoated_codes <- c(45760762)
femur_prosthesis_coated_codes <- c(45772469)

# udi
depuy_self_centering_udi_codes <- c(10603295003359,
                                    10603295003397,
                                    10603295003335,
                                    10603295003366,
                                    10603295003373,
                                    10603295003434,
                                    10603295003342,
                                    10603295003380,
                                    10603295003410,
                                    10603295003403,
                                    10603295003311,
                                    10603295003458,
                                    10603295003427) |> as.character()
depuy_biolox_delta_udi_codes <- c(10603295033448,
                                  10603295033288,
                                  10603295033455,
                                  10603295033431,
                                  10603295033622,
                                  10603295033615,
                                  10603295033271,
                                  10603295033295,
                                  10603295033639)
zimmer_biolox_delta_udi_codes <- c(00889024430327,
                                   00889024430310,
                                   00889024430334) |> as.character()
depuy_articul_eze_udi_codes <- c(10603295033103,
                                 10603295033066,
                                 10603295033189,
                                 10603295033172,
                                 10603295033196,
                                 10603295033127,
                                 10603295033080,
                                 10603295033349,
                                 10603295033356) |> as.character()
biomet_hip_system_udi_codes <- c(00887868308277,
                                 00880304197169) |> as.character()

depuy_c_stem <- c(10603295059608,
                  10603295059615,
                  10603295059592,
                  10603295059639,
                  10603295060307,
                  10603295060291,
                  10603295060284,
                  10603295060321,
                  10603295059646) |> as.character()
depuy_corail <- c(10603295168683,
                  10603295168676,
                  10603295168669,
                  10603295168690,
                  10603295168706,
                  10603295168768,
                  10603295168775,
                  10603295168713,
                  10603295168898) |> as.character()

exactech_acumatch_l_series <- c(10885862009524) |> as.character()

# export codelists
list(hip_fx = hip_fx_codes,
     hip_oa = hip_oa_codes,
     hemi = hemi_codes,
     hemi_cemented = hemi_cemented_codes,
     hemi_uncemented = hemi_uncemented_codes,
     hemi_revision = hemi_revision_codes,
     hemi_cemented_revision = hemi_cemented_revision_codes,
     hemi_uncemented_revision = hemi_uncemented_revision_codes,
     hip_replacement = hip_replacement_codes,
     hip_revision = hip_revision_codes,
     femoral_head = femoral_head_codes,
     femoral_head_bipolar = femoral_head_bipolar_codes,
     femoral_head_ceramic = femoral_head_ceramic_codes,
     femoral_head_metallic = femoral_head_metallic_codes,
     femur_prosthesis_codes = femur_prosthesis_codes,
     femur_prosthesis_uncoated = femur_prosthesis_uncoated_codes,
     femur_prosthesis_coated = femur_prosthesis_coated_codes) |>
  newCodelist() |>
  exportCodelist(here::here("hip", "cohorts","codelists"), type = "csv")

# export udi
readr::write_csv(tibble(udi = depuy_self_centering_udi_codes),
                 here::here("hip", "cohorts","udi", "depuy_self_centering_udi.csv"))
readr::write_csv(tibble(udi = depuy_biolox_delta_udi_codes),
                 here::here("hip", "cohorts","udi", "depuy_biolox_delta_udi.csv"))
readr::write_csv(tibble(udi = zimmer_biolox_delta_udi_codes),
                 here::here("hip", "cohorts","udi", "zimmer_biolox_delta_udi.csv"))
readr::write_csv(tibble(udi = depuy_articul_eze_udi_codes),
                 here::here("hip", "cohorts","udi", "depuy_articul_eze_udi.csv"))
readr::write_csv(tibble(udi = biomet_hip_system_udi_codes),
                 here::here("hip", "cohorts","udi", "biomet_hip_system_udi.csv"))
readr::write_csv(tibble(udi = depuy_c_stem),
                 here::here("hip", "cohorts","udi", "depuy_c_stem_udi.csv"))
readr::write_csv(tibble(udi = depuy_corail),
                 here::here("hip", "cohorts","udi", "depuy_corail_udi.csv"))




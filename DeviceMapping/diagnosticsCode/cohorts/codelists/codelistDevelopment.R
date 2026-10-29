
hip_fx_codes <- CodelistGenerator::getDescendants(cdm, 433856)

hemi_codes <- c(4290407, # maps from W46.1 (Primary prosthetic replacement of head of femur using cement)
                4077078, # maps from W47.1 (Primary prosthetic replacement of head of femur not using cement)
                44514853 # W48.1 (Primary prosthetic replacement of head of femur NEC)
)
hemi_cemented_codes <- 4290407
hemi_uncemented_codes <- 4077078

femoral_head_codes <- c(45763037, #Bipolar femoral head prosthesis
                        45761725, #Ceramic femoral head prosthesis
                        45758713 # Metallic femoral head prosthesis
)
femoral_head_bipolar_codes <- 45763037
femoral_head_ceramic_codes <- 45761725
femoral_head_metallic_codes <- 45758713

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
                                  10603295033639) |> as.character()
depuy_articul_eze_udi_codes <- c(10603295033103,
                                 10603295033066,
                                 10603295033189,
                                 10603295033172,
                                 10603295033196,
                                 10603295033127,
                                 10603295033080) |> as.character()

# export codelists
list(hip_fx = hip_fx_codes$concept_id,
     hemi = hemi_codes,
     hemi_cemented = hemi_cemented_codes,
     hemi_uncemented = hemi_uncemented_codes,
     femoral_head = femoral_head_codes,
     femoral_head_bipolar = femoral_head_bipolar_codes,
     femoral_head_ceramic = femoral_head_ceramic_codes,
     femoral_head_metallic = femoral_head_metallic_codes) |>
  newCodelist() |>
  exportCodelist(here::here("cohorts","codelists"), type = "csv")

# export udi
readr::write_csv(tibble(udi = depuy_self_centering_udi_codes),
                 here::here("cohorts","udi", "depuy_self_centering_udi.csv"))
readr::write_csv(tibble(udi = depuy_biolox_delta_udi_codes),
                 here::here("cohorts","udi", "depuy_biolox_delta_udi.csv"))
readr::write_csv(tibble(udi = depuy_articul_eze_udi_codes),
                 here::here("cohorts","udi", "depuy_articul_eze_udi_udi.csv"))



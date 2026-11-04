
codes <- importCodelist(here::here("hip", "cohorts", "codelists"))
udi_files <- list.files(path = here::here("hip","cohorts", "udi"),
                        pattern = "\\.csv$", full.names = TRUE)
udi <- udi_files |>
  set_names(tools::file_path_sans_ext(basename(udi_files))) |>
  purrr::map(~ readr::read_csv(.x, col_types = cols(.default = col_character())) |> pull("udi"))

# diagnoses -----
cdm$diagnoses <- conceptCohort(cdm,
                            conceptSet = codes[c("hip_fx", "hip_oa")],
                            name = "diagnoses",
                            exit = "event_start_date")

# procedures -----
cdm$procedures <- conceptCohort(cdm,
                          conceptSet = codes[str_detect(names(codes), "hemi|hip_replacement|hip_revision")],
                          name = "procedures",
                          exit = "event_start_date")

# device (index event)  -----
cdm$devices <- conceptCohort(cdm,
                            conceptSet = codes[str_detect(names(codes), "femoral_head|femur_prosthesis")],
                            name = "devices",
                            exit = "event_start_date")

# udi  -----
cdm$hip_udi <- customCohort(cdm,
                             table = "device_exposure",
                             field = "unique_device_id",
                             values = udi,
                             name = "hip_udi")

# bind -----
cdm <- bind(cdm$diagnoses,
            cdm$procedures,
            cdm$devices,
            cdm$hip_udi,
            name = "hip")
attr(cdm$hip, "cohort_set") <- attr(cdm$hip, "cohort_set") |>
  mutate(group = "hip")

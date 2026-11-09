
codes <- importCodelist(here::here("lens", "cohorts", "codelists"))
udi_files <- list.files(path = here::here("lens","cohorts", "udi"),
                        pattern = "\\.csv$", full.names = TRUE)
udi <- udi_files |>
  set_names(tools::file_path_sans_ext(basename(udi_files))) |>
  purrr::map(~ readr::read_csv(.x, col_types = cols(.default = col_character())) |> pull("udi"))

# diagnoses -----
cdm$diagnoses <- conceptCohort(cdm,
                            conceptSet = codes[c("cataract")],
                            name = "diagnoses",
                            exit = "event_start_date")

# procedures -----
cdm$procedures <- conceptCohort(cdm,
                          conceptSet = codes[str_detect(names(codes), "lens_implantation")],
                          name = "procedures",
                          exit = "event_start_date")

# device (index event)  -----
cdm$devices <- conceptCohort(cdm,
                            conceptSet = codes[names(codes) %in% c("lens", "lens_anterior", "lens_posterior")],
                            name = "devices",
                            exit = "event_start_date")

# udi  -----
cdm$lens_udi <- customCohort(cdm,
                            table = "device_exposure",
                            field = "unique_device_id",
                            values = udi,
                            name = "lens_udi")

# bind -----
cdm <- bind(cdm$diagnoses,
            cdm$procedures,
            cdm$devices,
            cdm$lens_udi,
            name = "lens")
attr(cdm$lens, "cohort_set") <- attr(cdm$lens, "cohort_set") |>
  mutate(group = "lens")

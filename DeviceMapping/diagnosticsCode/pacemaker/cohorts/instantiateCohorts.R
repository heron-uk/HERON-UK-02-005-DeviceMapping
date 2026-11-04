
codes <- importCodelist(here::here("pacemaker", "cohorts", "codelists"))
udi_files <- list.files(path = here::here("pacemaker","cohorts", "udi"),
                        pattern = "\\.csv$", full.names = TRUE)
udi <- udi_files |>
  set_names(tools::file_path_sans_ext(basename(udi_files))) |>
  purrr::map(~ readr::read_csv(.x, col_types = cols(.default = col_character())) |> pull("udi"))

# diagnoses -----
cdm$diagnoses <- conceptCohort(cdm,
                            conceptSet = codes[c("arrhythmia")],
                            name = "diagnoses",
                            exit = "event_start_date")

# procedures -----
cdm$procedures <- conceptCohort(cdm,
                          conceptSet = codes[str_detect(names(codes), "pacemaker_implantation")],
                          name = "procedures",
                          exit = "event_start_date")

# device (index event)  -----
cdm$devices <- conceptCohort(cdm,
                            conceptSet = codes[names(codes) %in% c("pacemaker", "pacemaker_dual", "pacemaker_single")],
                            name = "devices",
                            exit = "event_start_date")

# udi  -----
cdm$pacemaker_udi <- customCohort(cdm,
                             table = "device_exposure",
                             field = "unique_device_id",
                             values = udi,
                             name = "pacemaker_udi")


# bind -----
cdm <- bind(cdm$diagnoses,
            cdm$procedures,
            cdm$devices,
            cdm$pacemaker_udi,
            name = "pacemaker")
attr(cdm$pacemaker, "cohort_set") <- attr(cdm$pacemaker, "cohort_set") |>
  mutate(group = "pacemaker")

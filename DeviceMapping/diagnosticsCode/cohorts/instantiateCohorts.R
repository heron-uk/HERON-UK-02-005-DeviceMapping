
codes <- importCodelist(here::here("cohorts", "codelists"))
udi_files <- list.files(path = here::here("cohorts", "udi"),
                        pattern = "\\.csv$", full.names = TRUE)
udi <- udi_files |>
  set_names(tools::file_path_sans_ext(basename(udi_files))) |>
  purrr::map(~ readr::read_csv(.x, col_types = cols(.default = col_character())) |> pull("udi"))

# hip fx -----
cdm$hip_fx <- conceptCohort(cdm,
                            conceptSet = codes["hip_fx"],
                            name = "hip_fx",
                            exit = "event_start_date")

# hemi -----
cdm$hemi <- conceptCohort(cdm,
                          conceptSet = codes[str_detect(names(codes), "hemi")],
                          name = "hemi",
                          exit = "event_start_date")

cdm$hemi_post_fx <- cdm$hemi |>
  requireIsFirstEntry(name = "hemi_post_fx") |>
  requireCohortIntersect("hip_fx",
                         name = "hemi_post_fx",
                         window = c(-Inf, 0)) |>
  requireAge(c(60, 150),
             name = "hemi_post_fx")
attr(cdm$hemi_post_fx, "cohort_set") <- attr(cdm$hemi_post_fx, "cohort_set") |>
  mutate(cohort_name = paste0(cohort_name, "_post_fx"))

# device  -----
cdm$femoral_head <- conceptCohort(cdm,
                                  conceptSet = codes[str_detect(names(codes), "femoral_head")],
                                  name = "femoral_head",
                                  exit = "event_start_date")
# udi  -----
cdm$hip_udi <- customCohort(cdm,
                             table = "device_exposure",
                             field = "unique_device_id",
                             values = udi,
                             name = "hip_udi")

# bind -----
cdm <- bind(cdm$hip_fx,
            cdm$hemi,
            cdm$hemi_post_fx,
            cdm$femoral_head,
            cdm$hip_udi,
            name = "study_cohorts")

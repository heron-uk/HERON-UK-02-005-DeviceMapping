logMessage("Table summary")
results[["tbl_summary"]] <- summariseClinicalRecords(cdm = cdm,
                                                     dateRange = study_period,
                                                     omopTableName = "device_exposure")

logMessage("Trend - overall")
results[["tbl_trend"]] <- summariseTrend(cdm = cdm,
                                         dateRange = study_period,
                                         event = "device_exposure",
                                         interval = "years")

mapped <- c(TRUE, FALSE)
udi <-  c(TRUE, FALSE)
for(i in seq_along(mapped)){
  for(j in seq_along(udi)){

    working_cdm <- cdm
    working_mapped <- mapped[[i]]
    working_udi<- udi[[j]]

    cli::cli_inform("Trend - mapped {working_mapped} and udi {working_udi}")

    if(isTRUE(working_mapped)){
      # has mapping
      working_cdm$device_exposure <- working_cdm$device_exposure |>
        dplyr::filter(!is.na(device_concept_id) & device_concept_id != 0)
    } else {
      # not mapped
      working_cdm$device_exposure <- working_cdm$device_exposure |>
        dplyr::filter(is.na(device_concept_id) | device_concept_id == 0)
    }

    if(isTRUE(working_udi)){
      # has udi
      working_cdm$device_exposure <- working_cdm$device_exposure |>
        dplyr::filter(!is.na(unique_device_id) & unique_device_id != "")
    } else {
      # no udi
      working_cdm$device_exposure <- working_cdm$device_exposure |>
        dplyr::filter(is.na(unique_device_id) | unique_device_id == "")
    }

    results[[paste0("tbl_trend", working_mapped, working_udi)]] <- summariseTrend(cdm = working_cdm,
                                                                                  event = "device_exposure",
                                                                                  interval = "years")
    attr(results[[paste0("tbl_trend", working_mapped, working_udi)]],
         "settings") <- attr(results[[paste0("tbl_trend", working_mapped, working_udi)]],
                             "settings") |>
      mutate(mapped_to_standard = working_mapped,
             has_udi = working_udi)

  }}

getIncidencePlot <- function(res, outcome){
  res |>
    filterAdditional(analysis_interval == "years") |>
    filterGroup(outcome_cohort_name %in% outcome) |>
    filterSettings(denominator_age_group %in% c("10 to 150", "10 to 17",
                                                "18 to 29", "30 to 39",             
                                                "40 to 49", "50 to 150"),
                   denominator_time_at_risk %in% "0 to Inf") |>
    splitAdditional() |>
    mutate("incidence_start_date" = gsub("-01-01", "", incidence_start_date)) |>
    uniteAdditional(cols = additionalColumns(res)) |>
    mutate("cdm_name" = factor(cdm_name, levels = c("DK-DHR", "FinOMOP-THL", "SIDIAP", "IPCI", "CPRD GOLD"))) |>
    arrange(cdm_name) |>
    scatterPlot(
      facet = "cdm_name",
      colour = "denominator_age_group",
      x = "incidence_start_date",
      y = "incidence_100000_pys",
      ymin = "incidence_100000_pys_95CI_lower",
      ymax = "incidence_100000_pys_95CI_upper",
      line = FALSE,
      ribbon = FALSE,
      point = TRUE,   
      style = "darwin"
    ) +
    geom_line(
      aes(
        x = incidence_start_date,
        y = incidence_100000_pys,
        colour = denominator_age_group,
        linetype = denominator_age_group,
        group = denominator_age_group
      )
    ) +
    scale_linetype_manual(
      values = c(
        "10 to 150" = "dashed",
        "10 to 17" = "solid",
        "18 to 29" = "solid",
        "30 to 39" = "solid",
        "40 to 49" = "solid",
        "50 to 150" = "solid"
      ),
      breaks = c("10 to 150", "10 to 17", "18 to 29",   "30 to 39",   "40 to 49",  "50 to 150"),
      labels = c("10 to 150" = "Overall", "10 to 17", "18 to 29",   "30 to 39",   "40 to 49","50 to 150" = "≥50")
    ) +
    labs(y = "Incidence (100,000 person-years)", colour = "", fill = "", linetype = "") +
    theme(legend.position = "top",
          axis.text.x = element_text(angle = 90, hjust = 1))  +
    scale_shape_manual(
      values = c(
        "10 to 150" = 24, 
        "10 to 17" = 16,  
        "18 to 29" = 16,
        "30 to 39" = 16,
        "40 to 49" = 16,
        "50 to 150" = 16
      ),
      breaks = c("10 to 150", "10 to 17", "18 to 29",   "30 to 39",   "40 to 49",  "50 to 150"),
      labels = c("10 to 150" = "Overall", "10 to 17", "18 to 29",   "30 to 39",   "40 to 49","50 to 150" = "≥50")
    ) +
    scale_color_manual(
      values = c(
        "10 to 150" = "black",
        "10 to 17"      = "#D72000FF",
        "18 to 29"    = "#E2853AFF",
        "30 to 39"   = "#FFAD0AFF",
        "40 to 49"     = "#859B6CFF",
        "50 to 150"     = "#1F83B4FF"
      ),
      breaks = c("10 to 150", "10 to 17", "18 to 29",   "30 to 39",   "40 to 49",  "50 to 150"),
      labels = c("10 to 150" = "Overall", "10 to 17", "18 to 29",   "30 to 39",   "40 to 49",  "50 to 150" = "≥50"),
    ) +
    scale_fill_manual(
      values = c(
        "10 to 150" = "black",
        "10 to 17"      = "#D72000FF",
        "18 to 29"    = "#E2853AFF",
        "30 to 39"   = "#FFAD0AFF",
        "40 to 49"     = "#859B6CFF",
        "50 to 150"     = "#1F83B4FF"
      ),
      breaks = c("10 to 150", "10 to 17", "18 to 29",   "30 to 39",   "40 to 49",  "50 to 150"),
      labels = c("10 to 150" = "Overall", "10 to 17", "18 to 29",   "30 to 39",   "40 to 49","50 to 150" = "≥50")
    ) 
}

getSurvivalPlot <- function(res, outcome, years){
  
  if(outcome %in% c("myocardial_infarction_outcome",
                    "ischaemic_stroke_outcome",
                    "stroke_outcome",
                    "vte_outcome")) {
    res <- res |>
      filterSettings(outcome_washout == "90")
  } else {
  }
  
  res |>
    filter(variable_level == !!outcome) |>
    filter(as.numeric(additional_level) < 365*years) |>
    filterStrata(age_group %in% c("overall", "10-17", "18-29", "30-39", "40-49", "≥50")) |>
    mutate("cdm_name" = factor(cdm_name, levels = c("DK-DHR", "FinOMOP-THL", "SIDIAP", "IPCI", "CPRD GOLD"))) |>
    arrange(cdm_name) |>
    
    plotSurvival(cumulativeFailure = TRUE, 
                 colour = "age_group", 
                 facet = "cdm_name",
                 ribbon = TRUE,
                 timeScale = "years") +
    theme(legend.position = "top") +
    scale_color_manual(
      values = c(
        "overall" = "black",
        "10-17"      = "#D72000FF",
        "18-29"    = "#E2853AFF",
        "30-39"   = "#FFAD0AFF",
        "40-49"     = "#859B6CFF",
        "≥50"     = "#1F83B4FF"
      ),
      breaks = c("overall", "10-17", "18-29",   "30-39",   "40-49",  "≥50"),
      labels = c("overall" = "Overall", "10-17", "18-29",   "30-39",   "40-49",  "≥50")
    ) +
    scale_fill_manual(
      values = c(
        "overall" = "black",
        "10-17"      = "#D72000FF",
        "18-29"    = "#E2853AFF",
        "30-39"   = "#FFAD0AFF",
        "40-49"     = "#859B6CFF",
        "≥50"     = "#1F83B4FF"
      ),
      breaks = c("overall", "10-17", "18-29",   "30-39",   "40-49",  "≥50"),
      labels = c("overall" = "Overall", "10-17", "18-29",   "30-39",   "40-49",  "≥50")
    ) +
    aes(linetype = age_group) +
    scale_linetype_manual(
      values = c(
        "overall" = "dashed",
        "10-17" = "solid",
        "18-29" = "solid",
        "30-39" = "solid",
        "40-49" = "solid",
        "≥50"  = "solid"
      ),
      breaks = c("overall", "10-17", "18-29",   "30-39",   "40-49",  "≥50"),
      labels = c("overall" = "Overall", "10-17", "18-29",   "30-39",   "40-49",  "≥50")
    ) +
    labs(colour = "", linetype = "")
}


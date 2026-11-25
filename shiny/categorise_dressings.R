library(dplyr)
library(stringr)
library(omopgenerics)

flag_dressings <- function(df, pattern, col_name) {
  dressing_pattern <- regex(
    "dressing|pad|ribbon|rope|mesh|barrier|alginate|foam|charcoal|hydrocolloid|gauze|tulle|sheet|strip|collagen|cavity|fib(re|er)|matrix|film|pack(ing)?|wound|acticoat|aquacel|mepilex|allevyn|biatain|urgotul|promogran|actisorb|silvercel|kerracel|kerracontact|silverlon|calgitrol|inadine|iodoflex|iodosorb|povitulle|activon|actilite|algivon|medihoney|mesitran|revamil|sorbact|kendall amd|kerlix amd",
    ignore_case = TRUE
  )
  
  # Note: \\bgel\\b(?!\\s*sheet) excludes wound gels/tubes but keeps "gel sheet" dressings (e.g. Medihoney Gel Sheet)
  exclude_pattern <- regex(
    "test(ing)? strip|antigen|lateral flow|swab|nitrate|caustic|pencil|applicator|catheter|\\b(eye|ear|mouth|throat)\\b|lozenge|solution|cream|ointment|spray|paste|\\bgel\\b(?!\\s*sheet)|hydrogel|laryvox|filter|cassette|granufoam|v\\.a\\.c\\.|lifesaver|paint|scrub|wash|irrigation|rinse|powder|tube",
    ignore_case = TRUE
  )
  
  df |>
    mutate(
      .dev = coalesce(as.character(device_name), ""),
      .vmp = coalesce(as.character(dmd_vmp_name), ""),
      "{col_name}" := (str_detect(.dev, pattern) | str_detect(.vmp, pattern)) &
        (str_detect(.dev, dressing_pattern) | str_detect(.vmp, dressing_pattern)) &
        !str_detect(.dev, exclude_pattern) &
        !str_detect(.vmp, exclude_pattern)
    ) |>
    select(-.dev, -.vmp)
}
flag_silver_dressing <- function(df) {
  silver_pattern <- regex(
    "silv|\\bag\\b|\\bssd\\b|acticoat|promogran prisma|actisorb (silver|plus)|kerracontact|calgitrol",
    ignore_case = TRUE
  )
  cli::cli_inform("Adding is_silver_dressing column")
  flag_dressings(df, silver_pattern, "is_silver_dressing")
}
flag_iodine_dressing <- function(df) {
  iodine_pattern <- regex(
    "iodin|povidone|cadexomer|inadine|iodoflex|iodosorb|povitulle",
    ignore_case = TRUE
  )
  cli::cli_inform("Adding is_iodine_dressing column")
  flag_dressings(df, iodine_pattern, "is_iodine_dressing")
}
flag_honey_dressing <- function(df) {
  honey_pattern <- regex(
    "honey|manuka|activon|actilite|algivon|medihoney|mesitran|revamil|melmax|melladerm",
    ignore_case = TRUE
  )
  cli::cli_inform("Adding is_honey_dressing column")
  flag_dressings(df, honey_pattern, "is_honey_dressing")
}
flag_phmb_dressing <- function(df) {
  phmb_pattern <- regex(
    "\\bphmb\\b|polyhexanide|polyhexamethylene|kendall amd|kerlix amd",
    ignore_case = TRUE
  )
  cli::cli_inform("Adding is_phmb_dressing column")
  flag_dressings(df, phmb_pattern, "is_phmb_dressing")
}
flag_dacc_dressing <- function(df) {
  dacc_pattern <- regex(
    "\\bdacc\\b|dialkylcarbamoyl|sorbact",
    ignore_case = TRUE
  )
  cli::cli_inform("Adding is_dacc_dressing column")
  flag_dressings(df, dacc_pattern, "is_dacc_dressing")
}
flag_antimicrobial_dressings <- function(df) {
  df |>
    flag_silver_dressing() |>
    flag_iodine_dressing() |>
    flag_honey_dressing() |>
    flag_phmb_dressing() |>
    flag_dacc_dressing() |>
    mutate(
      is_antimicrobial_dressing = is_silver_dressing |
        is_iodine_dressing |
        is_honey_dressing |
        is_phmb_dressing |
        is_dacc_dressing
    )
}

devices <- readr::read_csv(here::here("devices.csv")) |> unique()

pc_devices <- devices |> 
  filter(is_medical_device == "yes") |> 
  filter(str_detect(tolower(cdm_name), "aurum|gold")) |> 
  select(cdm_name, 
         device_concept_id,
         device_name,
         dmd_vmp_name)

pc_devices <- pc_devices |> 
  flag_antimicrobial_dressings()

# codelist for each
list(silver_dressing = pc_devices |> 
  filter(is_silver_dressing) |> 
  pull(device_concept_id) |> 
  unique()) |> 
  exportCodelist(here::here("codelists"), type = "csv")
list(iodine_dressing = pc_devices |> 
       filter(is_iodine_dressing) |> 
       pull(device_concept_id) |> 
       unique()) |> 
  exportCodelist(here::here("codelists"), type = "csv")
list(honey_dressing = pc_devices |> 
       filter(is_honey_dressing) |> 
       pull(device_concept_id) |> 
       unique()) |> 
  exportCodelist(here::here("codelists"), type = "csv")
list(phmb_dressing = pc_devices |> 
       filter(is_phmb_dressing) |> 
       pull(device_concept_id) |> 
       unique()) |> 
  exportCodelist(here::here("codelists"), type = "csv")
list(dacc_dressing = pc_devices |> 
       filter(is_dacc_dressing) |> 
       pull(device_concept_id) |> 
       unique()) |> 
  exportCodelist(here::here("codelists"), type = "csv")
list(antimicrobial_dressing = pc_devices |> 
       filter(is_antimicrobial_dressing) |> 
       pull(device_concept_id) |> 
       unique()) |> 
  exportCodelist(here::here("codelists"), type = "csv")

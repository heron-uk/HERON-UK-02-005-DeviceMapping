
library(dplyr)
library(ggplot2)
library(treemapify)

load(here::here("data", "studyData.RData"))
devices <- readr::read_csv(here::here("devices.csv"))


working_data <- data[["summarise_table"]]|> 
  omopgenerics::tidy() |> 
  left_join(devices |> 
              select(!"device_name") |> 
              mutate(device_concept_id = as.character(device_concept_id)),
            by = join_by(cdm_name, device_concept_id,
                         device_concept_id_name, unique_device_id))



working_data |> 
  filter_out(device_concept_id == "0" & is.na(gudid_brand_name)) |> 
  # filter(cdm_name != "CPRD GOLD") |> 
  group_by(cdm_name, medical_specialty) |> 
  summarise(total_count = sum(count, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(area = total_count, fill = medical_specialty, label = medical_specialty)) +
  geom_treemap() +
  geom_treemap_text(
    colour = "white",
    place = "centre",
    grow = TRUE,
    reflow = TRUE
  ) +
  labs(title = "Counts by Medical Specialty") +
  theme(legend.position = "bottom") +
  facet_wrap(vars(cdm_name))

# 1. Assign a base color per specialty
library(colorspace)
library(scales)
specialties <- sort(unique(working_data$medical_specialty))
base_palette <- setNames(hue_pal(l = 65, c = 100)(length(specialties)), specialties)

# 2. Compute exact shade per (medical_specialty, device_class)
plot_data <- working_data |>  
  filter_out(device_concept_id == "0" & is.na(gudid_brand_name)) |> 
  group_by(cdm_name, medical_specialty, device_class) |> 
  summarise(total_count = sum(count, na.rm = TRUE), .groups = "drop") |> 
  mutate(
    device_class = factor(
      device_class, 
      levels = c("Class I", "Class IIa", "Class IIb", "Class III")
    ),
    base_col = base_palette[medical_specialty],
    shade_amount = case_when(
      device_class == "Class I"   ~ -0.35, # Lighter
      device_class == "Class IIa" ~ -0.10,
      device_class == "Class IIb" ~  0.15,
      device_class == "Class III" ~  0.40  # Darker
    ),
    tile_fill = darken(base_col, amount = shade_amount)
  )

ggplot(plot_data, aes(
  area = total_count,
  fill = tile_fill,
  subgroup = medical_specialty,
  label = device_class
)) +
  geom_treemap(colour = "white", size = 0.8) +
  geom_treemap_subgroup_border(colour = "white", size = 2.5) +
  geom_treemap_subgroup_text(
    place = "topleft",
    fontface = "bold",
    colour = "white",
    size = 12
  ) +
  geom_treemap_text(
    place = "bottomright",
    colour = "white",
    size = 9,
    reflow = TRUE
  ) +
  scale_fill_identity() +
  theme_void() +
  facet_wrap(vars(cdm_name))

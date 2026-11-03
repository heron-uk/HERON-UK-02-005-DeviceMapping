
# diagnoses
cataract_codes <-  CodelistGenerator::getDescendants(cdm, 375545)$concept_id

# procedure codes
lens_implantation_codes <- CodelistGenerator::getDescendants(cdm, 4288992)$concept_id

# device snomed
lens_codes <- c(45758993, # Posterior-chamber intraocular lens, pseudophakic
                45758380 # Anterior-chamber intraocular lens, pseudophakic
)
lens_posterior_codes <- 45758993
lens_anterior_codes <- 45758380

# udi


# export codelists
list(cataract = cataract_codes,
     lens_implantation = lens_implantation_codes,
     lens = lens_codes,
     lens_posterior = lens_posterior_codes,
     lens_anterior = lens_anterior_codes) |>
  newCodelist() |>
  exportCodelist(here::here("lens", "cohorts","codelists"), type = "csv")

# export udi



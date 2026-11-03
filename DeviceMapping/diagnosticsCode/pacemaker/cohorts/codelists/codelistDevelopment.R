
# diagnoses
arrhythmia_codes <- CodelistGenerator::getDescendants(cdm, 44784217)$concept_id

# procedure codes
pacemaker_implantation_codes <- CodelistGenerator::getDescendants(cdm, c(4051938))$concept_id

# device snomed
pacemaker_codes <- c(45772840, # Implantable cardiac pacemaker
                     2615795, # Pacemaker, dual chamber, rate-responsive (implantable)
                     2615796 # Pacemaker, single chamber, rate-responsive (implantable)
)
pacemaker_dual_codes <- 2615795
pacemaker_single_codes <- 2615796



# udi


# export codelists
list(arrhythmia = arrhythmia_codes,
     pacemaker_implantation = pacemaker_implantation_codes,
     pacemaker = pacemaker_codes,
     pacemaker_dual = pacemaker_dual_codes,
     pacemaker_single = pacemaker_single_codes) |>
  newCodelist() |>
  exportCodelist(here::here("pacemaker", "cohorts","codelists"), type = "csv")

# export udi



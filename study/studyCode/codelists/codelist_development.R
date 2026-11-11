
hip_oa_codes <- CodelistGenerator::getDescendants(cdm, 4079749)
hip_fx_codes <- CodelistGenerator::getDescendants(cdm, 433856)
cataracts <-  CodelistGenerator::getDescendants(cdm, 375545)
arrhythmia <- CodelistGenerator::getDescendants(cdm, 44784217)

hip_replacement <- CodelistGenerator::getDescendants(cdm, c(4203771, 4290407, 4134857))
lens_implantation <- CodelistGenerator::getDescendants(cdm, 4288992)
pacemaker_implantation <- CodelistGenerator::getDescendants(cdm, c(4051938))

list(hip_oa = hip_oa_codes$concept_id,
     hip_fx = hip_fx_codes$concept_id,
     hip_replacement = hip_replacement$concept_id,
     cataracts = cataracts$concept_id,
     lens_implantation = lens_implantation$concept_id,
     arrhythmia = arrhythmia$concept_id,
     pacemaker_implantation = pacemaker_implantation$concept_id
     ) |>
  newCodelist() |>
  exportCodelist(here::here("codelists"), type = "csv")

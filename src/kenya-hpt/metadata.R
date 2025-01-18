library(tidyverse)
library(data.table)
library(datimutils)

datimutils::loginToDATIM(username = username,
                         password = password,
                         base_url = base.url)

hpt_des <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:in:[ppdqEN91qZs]", 
                                   fields = "name, id, shortName, description, formName,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  select(name, id, shortName, formName,domainType, valueType, categoryCombo.id) %>%
  mutate(
    main_category = case_when(#Medicines
      str_detect(shortName, "Adrenaline") ~ "Medicines",
      str_detect(shortName, "Albendazole") ~ "Medicines",
      str_detect(shortName, "Amlodipine") ~ "Medicines",
      str_detect(shortName, "Amoxicillin") ~ "Medicines",
      str_detect(shortName, "Atorvastatin") ~ "Medicines",
      str_detect(shortName, "Benzyl penicillin") ~ "Medicines",
      str_detect(shortName, "Carbamazepine") ~ "Medicines",
      str_detect(shortName, "Ceftriaxone") ~ "Medicines",
      str_detect(shortName, "Cetirizine") ~ "Medicines",
      str_detect(shortName, "Chlorhexidine") ~ "Medicines",
      str_detect(shortName, "Chlorpheniramine") ~ "Medicines",
      str_detect(shortName, "Fluoxetine") ~ "Medicines",
      str_detect(shortName, "Gliclazide") ~ "Medicines",
      str_detect(shortName, "Haloperidol") ~ "Medicines",
      str_detect(shortName, "Hydrocortisone") ~ "Medicines",
      str_detect(shortName, "Hydroxyurea") ~ "Medicines",
      str_detect(shortName, "Insulin, Premix") ~ "Medicines",
      str_detect(shortName, "Soluble Insulin") ~ "Medicines",
      str_detect(shortName, "Lignocaine") ~ "Medicines",
      str_detect(shortName, "Losartan") ~ "Medicines",
      str_detect(shortName, "Magnesium") ~ "Medicines",
      str_detect(shortName, "Metformin") ~ "Medicines",
      str_detect(shortName, "Metronidazole") ~ "Medicines",
      str_detect(shortName, "Midazolam") ~ "Medicines",
      str_detect(shortName, "Nystatin") ~ "Medicines",
      str_detect(shortName, "Omeprazole") ~ "Medicines",
      str_detect(shortName, "ORS Co-Pack") ~ "Medicines",
      str_detect(shortName, "Oxytocin") ~ "Medicines",
      str_detect(shortName, "Paracetamol") ~ "Medicines",
      str_detect(shortName, "Oxytocin") ~ "Medicines",
      str_detect(shortName, "Penicillin") ~ "Medicines",
      str_detect(shortName, "Salbutamol") ~ "Medicines",
      str_detect(shortName, "Sodium") ~ "Medicines",
      str_detect(shortName, "Tetracycline") ~ "Medicines",
      
      # Medical Supplies
      str_detect(shortName, "Autoclaving") ~ "Medical Supplies",
      str_detect(shortName, "Bandage") ~ "Medical Supplies",
      str_detect(shortName, "Bin Liners") ~ "Medical Supplies",
      str_detect(shortName, "Catheter") ~ "Medical Supplies",
      str_detect(shortName, "Cord Clamp") ~ "Medical Supplies",
      str_detect(shortName, "Cotton") ~ "Medical Supplies",
      str_detect(shortName, "Gloves") ~ "Medical Supplies",
      str_detect(shortName, "syringes") ~ "Medical Supplies",
      str_detect(shortName, "cannula") ~ "Medical Supplies",
      str_detect(shortName, "infusion") ~ "Medical Supplies",
      str_detect(shortName, "Maternity Pad") ~ "Medical Supplies",
      str_detect(shortName, "Nasal Prongs") ~ "Medical Supplies",
      str_detect(shortName, "Feeding tube") ~ "Medical Supplies",
      str_detect(shortName, "Safety Boxes") ~ "Medical Supplies",
      str_detect(shortName, "Solusets") ~ "Medical Supplies",
      str_detect(shortName, "Suction Catheter") ~ "Medical Supplies",
      str_detect(shortName, "Surgical Blade") ~ "Medical Supplies",
      str_detect(shortName, "Suture") ~ "Medical Supplies",
      str_detect(shortName, "Syringe") ~ "Medical Supplies",
      str_detect(shortName, "Strapping") ~ "Medical Supplies",
      
      
      str_detect(shortName, "Anti-Human") ~ "Diagnostics",
      str_detect(shortName, "Blood Grouping") ~ "Diagnostics",
      str_detect(shortName, "Distilled water") ~ "Diagnostics",
      str_detect(shortName, "Formaldehyde") ~ "Diagnostics",
      str_detect(shortName, "Glucose Test") ~ "Diagnostics",
      str_detect(shortName, "Glycerol") ~ "Diagnostics",
      str_detect(shortName, "Pylori Strips") ~ "Diagnostics",
      str_detect(shortName, "Haemoglobin Cuvettes") ~ "Diagnostics",
      str_detect(shortName, "Lugol’s iodine") ~ "Diagnostics",
      str_detect(shortName, "Test Kit") ~ "Diagnostics",
      str_detect(shortName, "test strips") ~ "Diagnostics"
    )
  )

fwrite(hpt_des, paste0("data/kenya-hpt/metadata/HPT_DES.csv"))

#-----------------------------------------------------------------------
source("src/kenya-itt/packages.R")
source("src/kenya-itt/functions.R")
source("src/kenya-itt/setup.R")
login(username, password, base.url)
#--------------------------------------------------------------------
# All datasets 
datasets <- datimutils::getMetadata("dataSets", 
                                    fields = "id,name") |>
  dplyr::arrange(name)

#dbWriteTable(con, "datasets", datasets, overwrite = TRUE)

# # All category options
# categoryoptions <- datimutils::getMetadata("categoryOptions", 
#                                            fields = "id,name") |>
#   dplyr::arrange(name)
# # All category option combos
# categoryoptioncombos <- datimutils::getMetadata("categoryOptionCombos", 
#                                                 fields = "id,name") |>
#   dplyr::arrange(name) |>
#   rename(coc_name = name)

indicators <- datimutils::getMetadata("indicators",
                                          fields = "id,name,numerator,denominator,indicatorGroups[id, name]") %>%
  rename(indicator_name = name,
         indicator_id = id) %>%
  unnest(indicatorGroups, keep_empty = TRUE) %>%
  rename(group = name,
         group_id = id)

imm_indicators <- indicators %>%
  select(-group_id) %>%
  filter(group %in% c("Immunisation Rev_2020"))



#-----------------------------------------------------------------------------
# Data Elements
# ----------------------------------------------------------------------------------------------------
fp_des <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:in:[g3RQRuh8ikd,UpS2bTVcClZ]", 
                                        fields = "name, id, shortName, description, formName,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  select(name, id, shortName, formName,domainType, valueType, categoryCombo.id) 

#fwrite(fp_des,  "data/kenya-itt/metadata/fp_des.csv")

# FP data elements
fp_des_clean <- fp_des |>
  mutate(
    programme = "Family Planning",
    product = case_when(str_detect(name, "Combined Oral contraceptive Pills") ~ "CoC pills",
                        str_detect(name, "Cycle Beads") ~ "Cycle Beads",
                        str_detect(name, "DMPA-IM") ~ "DMPA-IM",
                        str_detect(name, "DMPA-SC") ~ "DMPA-SC",
                        str_detect(name, "Emergency Contraceptive pills") ~ "EC Pills",
                        str_detect(name, "Female Condoms") ~ "Female Condoms",
                        str_detect(name, "Non-Hormonal IUCD") ~ "Non-Hormonal IUCD",
                        str_detect(name, "Implant \\(2-Rod\\) – LNG 75mg \\(5 years\\)") ~ "Implant(2-Rod) 5yr",
                        str_detect(name, "Implants \\(1-Rod\\) – ENG 68mg") ~ "Implants(1-Rod)",
                        str_detect(name, "Implants \\(2-Rod\\) - LNG 75mg \\(3 years\\)") ~ "Implant(2-Rod) 3yr",
                        str_detect(name, "Male Condoms") ~ "Male Condoms",
                        str_detect(name, "Progestin only pills") ~ "Progestin only pills",
                        str_detect(name, "Hormonal IUCD") ~ "Hormonal IUCD",
                        str_detect(name, "Others") ~ "Others",
                          
                        # Service mapping
                        str_detect(name, "MOH 747A_service_ Cycle Beads") ~ "Cycle Beads",
                        str_detect(name, "MOH 747A_service_1 Rod Implant \\(Etonogestrel 68mg\\)") ~ "Implants(1-Rod)",
                        str_detect(name, "MOH 747A_service_2 Rod Implant 3 years \\(Levonorgestrel 75mg\\)") ~ "Implant(2-Rod) 3yr",
                        str_detect(name, "MOH 747A_service_2 Rod Implant 5 years \\(Levonorgestrel 75mg\\)") ~ "Implant(2-Rod) 5yr",
                        str_detect(name, "MOH 747A_service_Combined Oral Contraceptive Pills \\(COC\\)") ~ "CoC pills",
                        str_detect(name, "MOH 747A_service_ Condoms, Male") ~ "Male Condoms",
                        str_detect(name, "MOH 747A_service_Condoms, Female") ~ "Female Condoms",
                        str_detect(name, "MOH 747A_service_Emergency Contraceptive Pills \\(EC\\)") ~ "EC Pills",  str_detect(name, "MOH 747A_service_Progestin Only Pills \\(POP\\)") ~ "Progestin only pills",
                        str_detect(name, "MOH 747A_service_DMPA Injection-150mg, IM") ~ "DMPA-IM",
                        str_detect(name, "MOH 747A_service_DMPA Injection-104mg/0.65ml, SC") ~ "DMPA-SC",
                        str_detect(name, "MOH 747A_service_IUD Hormonal") ~ "Hormonal IUCD",
                        str_detect(name, "MOH 747A_service_IUD non-hormonal \\(Copper T\\)") ~ "Non-Hormonal IUCD",
                        TRUE ~ "delete"),
    
    metric = case_when(str_detect(name, "MOH 747A_service_") ~ "services",
                        TRUE ~ "supply chain"),
    
    
  )  

fwrite(fp_des_clean,  "data/kenya-itt/metadata/fp_des_clean.csv")

# Immunization

imm_des <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:in:[XoHnrLBL1qB]", 
                                   fields = "name, id, shortName, description, formName,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  select(name, id, shortName, formName,domainType, valueType, categoryCombo.id)

#fwrite(imm_des,  "data/kenya-itt/metadata/imm_des.csv")

imm_des_clean <- imm_des|>
  mutate(
    programme = "Immunization",
    product = case_when(str_detect(name, "COVID") ~ "COVID-19",
                        str_detect(name, "BCG") ~ "BCG",
                          str_detect(name, "DPT") ~ "Penta",
                          str_detect(name, "HPV") ~ "HPV",
                          str_detect(name, "IPV") ~ "IPV",
                          str_detect(name, "Measles") ~ "Measles",
                          str_detect(name, "OPV") ~ "OPV",
                          str_detect(name, "Pneumococal") ~ "Pneumococal",
                          str_detect(name, "Rotavirus") ~ "Rotavirus",
                          str_detect(name, "Rota virus") ~ "Rotavirus",
                          str_detect(name, "Tetanus Toxoid") ~ "Tetanus Toxoid",
                          str_detect(name, "Yellow") ~ "Yellow Fever",
                          str_detect(name, "100,000 IU") ~ "Vitamin A 100,000",
                          str_detect(name, "200,000 IU") ~ "Vitamin A 200,000",
                          str_detect(name, "50,000 IU") ~ "Vitamin A 50,000",
                          TRUE ~ "delete"),
    metric = case_when(
      grepl("beginning", name, ignore.case = TRUE) ~ "Beginning Balance",
      grepl("end", name, ignore.case = TRUE) ~ "Ending Balance",
      grepl("Administered", name, ignore.case = TRUE) ~ "Administered",
      grepl("Adm", name, ignore.case = TRUE) ~ "Administered",
      grepl("Issued", name, ignore.case = TRUE) ~ "Administered",
      grepl("received within", name, ignore.case = TRUE) ~ "Received",
      grepl("Fully Immunized Child", name, ignore.case = TRUE) ~ "Fully Immunized Child",
      grepl("girls vaccinated", name, ignore.case = TRUE) ~ "Girls Vaccinated",
      TRUE ~ NA_character_)
  )



fwrite(imm_des_clean,  "data/kenya-itt/metadata/imm_des_clean.csv")

# Malaria
mal_des <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:in:[RRnz4uPHXdl]", 
                                   fields = "name, id, shortName, description, formName,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  select(name, id, shortName, formName,domainType, valueType, categoryCombo.id) 

#fwrite(mal_des,  "metadata/mal_des.csv")

mal_des_clean <- mal_des |>
  mutate(
    programme = "Malaria",
    product = case_when(str_detect(name, "Artemether-Lumefantrine 20/120 Tabs 12s") ~ "AL12",
                          str_detect(name, "Artemether-Lumefantrine 20/120 Tabs 18s") ~ "AL18",
                          str_detect(name, "Artemether-Lumefantrine 20/120 Tabs 24s") ~ "AL24",
                          str_detect(name, "Artemether-Lumefantrine 20/120 Tabs 6s") ~ "AL6",
                          str_detect(name, "Artesunate Injection") ~ "AS Injection",
                          str_detect(name, "Dihydroartemesinin/piperaquine 160mg Tabs") ~ "DHA/PPQ 160mg",
                          str_detect(name, "Dihydroartemesinin/piperaquine 320mg Tabs") ~ "DHA/PPQ 320mg",
                          str_detect(name, "Quinine \\(200mg\\) Tabs") ~ "Quinine (200mg)",
                          str_detect(name, "Quinine \\(300mg\\) Tabs") ~ "Quinine (300mg)",
                          str_detect(name, "Quinine inj \\(600mg/2ml\\) Amps") ~ "Quinine (600mg)",
                          str_detect(name, "Rapid Diagnostic Tests") ~ "RDTs",
                          str_detect(name, "Rapid Diagnostic Tests") ~ "SP Tabs",
                          TRUE ~ "delete"),
    metric = case_when(
      grepl("beginning", name, ignore.case = TRUE) ~ "Beginning Balance",
      grepl("end", name, ignore.case = TRUE) ~ "Ending Balance",
      grepl("Administered", name, ignore.case = TRUE) ~ "Administered",
      grepl("Adm", name, ignore.case = TRUE) ~ "Administered",
      grepl("Issued", name, ignore.case = TRUE) ~ "Administered",
      grepl("received within", name, ignore.case = TRUE) ~ "Received",
      grepl("Fully Immunized Child", name, ignore.case = TRUE) ~ "Fully Immunized Child",
      grepl("girls vaccinated", name, ignore.case = TRUE) ~ "Girls Vaccinated",
      TRUE ~ NA_character_)
  ) 

fwrite(mal_des_clean,  "data/kenya-itt/metadata/mal_des_clean.csv")

# Nutrition

nut_des <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:in:[mVRzpvT29MP]", 
                                   fields = "name, id, shortName, description, formName,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  select(name, id, shortName, formName,domainType, valueType, categoryCombo.id)

fwrite(nut_des,  "data/kenya-itt/metadata/nut_des.csv")


nut_des_clean <- nut_des |>
  mutate(
    programme = "Nutrition",
    product = case_when(str_detect(name, "Therapeutic Vitamin A 100 000 IU \\(30 mg RE\\)") ~ "Therapeutic Vitamin A 100 000 IU (30 mg RE)",
                          str_detect(name, "Therapeutic Vitamin A 200 000 IU \\(60 mg RE\\)") ~ "Therapeutic Vitamin A 200 000 IU (60 mg RE)",
                          str_detect(name, "Ready to use therapeutic food \\(RUTF\\) bar, 500kcal/100g") ~ "Ready to use therapeutic food (RUTF) bar, 500kcal/100g",
                          str_detect(name, "Ready to use therapeutic food \\(RUTF\\) paste, 500kcal/92g") ~ "Ready to use therapeutic food (RUTF) paste, 500kcal/92g",
                          str_detect(name, "Micronutrient powder") ~ "Micronutrient powder",
                          str_detect(name, "Multiple Vitamin and Mineral mix") ~ "Multiple Vitamin and Mineral mix",
                          str_detect(name, "Combined Iron \\(60mg\\) Folic Acid \\(400µg\\)") ~ "Combined Iron (60mg) Folic Acid (400µg)",
                          TRUE ~ "delete"),
    metric = NA_character_
  ) 


fwrite(nut_des_clean,  "data/kenya-itt/metadata/nut_des_clean.csv")


products <- bind_rows(fp_des_clean, imm_des_clean, mal_des_clean, nut_des_clean ) |>
  distinct(programme, product) |>
  filter(!product %in% c("delete", "Others"))

fwrite(products,  "data/kenya-itt/metadata/products.csv")
dbWriteTable(con, "products", products, overwrite = TRUE, row.names = FALSE)

#----------------------------------------------------------------------------------------------------
# Organisational units
OrgUnits <- datimutils::getMetadata("organisationUnits/HfVjCurKxh2&includeDescendants=true", 
                                    fields = "id,level,parent, code,name")
glimpse(OrgUnits)

# level 6
level_6 <- OrgUnits %>%
  filter(level == 6) %>%
  transmute(
    community.id = id,
    community.parent.id = parent.id,
    community = name
  )
glimpse(level_6)

# level 5
level_5 <- OrgUnits %>%
  filter(level == 5) %>%
  transmute(
    facility.id = id,
    facility.parent.id = parent.id,
    facility = name
  )
glimpse(level_5)

# level 4
level_4 <- OrgUnits %>%
  filter(level == 4) %>%
  transmute(
    ward.id = id,
    ward.parent.id = parent.id,
    ward = name
  )
glimpse(level_4)

# level 4
level_3 <- OrgUnits %>%
  filter(level == 3) %>%
  transmute(
    subcounty.id = id,
    subcounty.parent.id = parent.id,
    subcounty = name
  )
glimpse(level_3)

# level 4
level_2 <- OrgUnits %>%
  filter(level == 2) %>%
  transmute(
    county.id = id,
    county.parent.id = parent.id,
    county = name
  )
glimpse(level_2)

# level 4
level_1 <- OrgUnits %>%
  filter(level == 1) %>%
  transmute(
    country.id = id,
    country.parent.id = parent.id,
    country = name
  )

add_facility <- OrgUnits %>%
  left_join(level_5, by = c("parent.id" = "facility.id")) %>%
  mutate(
    facility.parent.id = case_when(is.na(facility.parent.id) & level == 5 ~ parent.id,
                                   TRUE ~ facility.parent.id),
    facility = case_when(is.na(facility) & level == 5 ~ name,
                         TRUE ~ facility),
    
  )

add_facility |> 
  filter(is.na(facility)) |> 
  group_by(level) |> 
  tally()

add_ward <- add_facility %>%
  left_join(level_4, by = c("facility.parent.id" = "ward.id")) %>%
  mutate(
    ward.parent.id = case_when(is.na(ward.parent.id) & level == 4 ~ parent.id,
                               TRUE ~ ward.parent.id),
    ward = case_when(is.na(ward) & level == 4 ~ name,
                     TRUE ~ ward),
    
  )

add_ward |> 
  filter(is.na(ward)) |> 
  group_by(level) |> 
  tally()

add_subcounty <- add_ward %>%
  left_join(level_3, by = c("ward.parent.id" = "subcounty.id")) %>%
  mutate(
    subcounty.parent.id = case_when(is.na(subcounty.parent.id) & level == 3 ~ parent.id,
                                    TRUE ~ subcounty.parent.id),
    subcounty = case_when(is.na(subcounty) & level == 3 ~ name,
                          TRUE ~ subcounty),
    
  )
  
add_subcounty |> 
  filter(is.na(subcounty)) |> 
  group_by(level) |> 
  tally()


add_county <- add_subcounty %>%
  left_join(level_2, by = c("subcounty.parent.id" = "county.id")) %>%
  mutate(
    county.parent.id = case_when(is.na(county.parent.id) & level == 2 ~ parent.id,
                                 TRUE ~ county.parent.id),
    county = case_when(is.na(county) & level == 2 ~ name,
                       TRUE ~ county),
    
  )
add_county |> 
  filter(is.na(county)) |> 
  group_by(level) |> 
  tally()



clean_orgs <- add_county  |> 
  mutate(
    ward = str_replace_all(ward, " Ward", ""),
    subcounty = str_replace_all(subcounty, " Sub County", ""),
    county = str_replace_all(county, " County", "")
  )
glimpse(clean_orgs)


fwrite(clean_orgs, "data/kenya-itt/metadata/orgunits.csv")

dbWriteTable(con, "orgunits", clean_orgs, overwrite = TRUE, row.names = FALSE)









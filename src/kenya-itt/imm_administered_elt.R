rm(list = ls())
project_path <- "~/projects/insupplyHealth/kenya_itt/data/kenya_itt/"
source(paste0(project_path, "setup.R"))
#-----------------------------------------------------------------------
# important!
categoryoptioncombos <- datimutils::getMetadata("categoryOptionCombos", 
                                                fields = "id,name") |>
  dplyr::arrange(name) |>
  rename(coc_name = name)

imm_dataelements <- fread(paste0(project_path, "metadata/imm_des_clean.csv")) |>
  select(-c(formName)) |>
  rename(de_name = name)

# results
imm_data <- load_program_data("Immunization", project_path)

imm_data <- imm_data %>%
  dtplyr::lazy_dt() %>%
  select(dataElement:value) %>%
  left_join(imm_dataelements, by = c("dataElement" = "id")) %>%
  left_join(categoryoptioncombos, by = c("categoryOptionCombo" = "id")) %>%
  filter(!product %in% c("delete", "Others")) %>%
  filter(valueType %in% c("NUMBER", "INTEGER", "INTEGER_ZERO_OR_POSITIVE")) %>%
  mutate(
    value = as.numeric(value),
    period = as.numeric(period)
  ) |>
  as.data.frame()


# Columns
imm_data |> filter(product == "Yellow Fever") |> group_by(de_name, coc_name)|>summarise(val=sum(value, na.rm = TRUE))

imm_adm <- imm_data |>
  select(-c(categoryOptionCombo, attributeOptionCombo, domainType, valueType, metric, shortName, categoryCombo.id,  programme)) |>
  filter(
    de_name %in%
      c(
        # BCG---------------------
        "MOH710 BCG doses Administered",
        
        # HPV----------------------
        "MOH710 HPV Vaccine Dose1(at 10 years)",
        "MOH710 HPV Vaccine Dose 2 (At least 6 months after HPV dose 1)",
        
        # IPV ---------------------
        "MOH710 IPV doses Administered",
        
        # Measles ---------------------
        "MOH710 Measles-Rubella 1 doses Administered",
        "MOH710 Measles-Rubella 1 doses Administered",
        "MOH710 Measles-Rubella 2 Dose Adm (at 1 1/2 - 2 years)",
        "MOH710 Measles-Rubella 2 Dose Administered >2 yrs",
        
        # OPV ---------------------
        "MOH710 OPV Birth doses Administered",
        "MOH710 OPV1 doses Administered",
        "MOH710 OPV2 doses Administered",
        "MOH710 OPV3 doses Administered",
        
        # Penta ---------------------
        "MOH710 DPT/Hep+HiB1 doses Administered",
        "MOH710 DPT/Hep+HiB2 doses Administered",
        "MOH710 DPT/Hep+HiB3 doses Administered",
        
        # Pneumococal ---------------------
        "MOH710 Pneumococal 1 doses Administered",
        "MOH710 Pneumococal 2 doses Administered",
        "MOH710 Pneumococal 3 doses Administered",
        
        # Rotavirus ---------------------
        "MOH710 Rotavirus 1 doses Administered",
        "MOH710 Rotavirus 2 doses Administered",
        "MOH710 Rotavirus 3 doses Administered",
        
        # Tetanus Toxoid ---------------------
        "MOH710 Tetanus Toxoid for Pregnant women",
        "MOH 710 Rev 2020_Tetanus Toxoid for Trauma",
        
        # Vitamin A 100,000 ---------------------
        "MOH710 Vitamin A at 6 - 11 months(100,000 IU)",
        
        # Vitamin A 200,000 ---------------------
        "MOH710 Vitamin A at 12 to 59 months (200,000 IU)",
        
        # Yellow Fever ---------------------
        "MOH710 Yellow fever doses Administered"
      )
  )|>
  mutate(
    dose_number = case_when(
      grepl("HPV Vaccine Dose1", de_name, ignore.case = TRUE) ~ 1,
      grepl("HPV Vaccine Dose 2", de_name, ignore.case = TRUE) ~ 2,
      grepl("Dose 2", de_name, ignore.case = TRUE) ~ 2,
      grepl("Dose 3", de_name, ignore.case = TRUE) ~ 3,
      grepl("OPV1", de_name, ignore.case = TRUE) ~ 1,
      grepl("OPV2", de_name, ignore.case = TRUE) ~ 2,
      grepl("OPV3", de_name, ignore.case = TRUE) ~ 3,
      grepl("HiB1", de_name, ignore.case = TRUE) ~ 1,
      grepl("HiB2", de_name, ignore.case = TRUE) ~ 2,
      grepl("HiB3", de_name, ignore.case = TRUE) ~ 3,
      grepl("Pneumococal 1", de_name, ignore.case = TRUE) ~ 1,
      grepl("Pneumococal 2", de_name, ignore.case = TRUE) ~ 2,
      grepl("Pneumococal 3", de_name, ignore.case = TRUE) ~ 3,
      grepl("Rubella 1", de_name, ignore.case = TRUE) ~ 1,
      grepl("Rubella 2", de_name, ignore.case = TRUE) ~ 2,
      grepl("Rotavirus 1", de_name, ignore.case = TRUE) ~ 1,
      grepl("Rotavirus 2", de_name, ignore.case = TRUE) ~ 2,
      grepl("Rotavirus 3", de_name, ignore.case = TRUE) ~ 3,
      TRUE ~ 0
    )
  ) |>
  mutate(
    dose_name = case_when(
      dose_number == 1 ~ paste0(product, " 1"),
      dose_number == 2 ~ paste0(product, " 2"),
      dose_number == 3 ~ paste0(product, " 3"),
      TRUE ~ product
    )
  )


adm_products <- imm_adm %>%
  distinct(product, dose_name)

dbWriteTable(con, 
             "product_dosage", 
             adm_products, 
             overwrite = TRUE, 
             row.names = FALSE)


dbWriteTable(con, 
             "vaccines_admin", 
             imm_adm, 
             overwrite = TRUE, 
             row.names = FALSE)

# Assuming the connection `con` is already established

# # Set the batch size
# batch_size <- 50000
# 
# # Get the number of rows in the data frame
# n <- nrow(imm_adm)
# 
# # Loop through the data frame in chunks
# for (i in seq(1, n, by = batch_size)) {
#   # Determine the start and end of the current chunk
#   chunk <- imm_adm[i:min(i+batch_size-1, n), ]
#   
#   # Use dbWriteTable to write the current chunk
#   dbWriteTable(con, 
#                name = "vaccines_admin", 
#                value = chunk, 
#                row.names = FALSE, 
#                append = ifelse(i == 1, FALSE, TRUE))  # Use overwrite=TRUE only for the first batch
# }

# Disconnect after the operation is complete
dbDisconnect(con)



















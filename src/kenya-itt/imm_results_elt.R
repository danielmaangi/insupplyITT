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
imm_transform <- imm_data |>
  dtplyr::lazy_dt() |>
  arrange(programme, orgUnit, product, period) |>
  group_by(programme, orgUnit, product, period) |>
  summarise(
    end_bal = sum(value[metric == "Ending Balance"], na.rm = TRUE),
    begin_bal = sum(value[metric == "Beginning Balance"], na.rm = TRUE),
    recieved = sum(value[metric %in% c("Received")], na.rm = TRUE),
    administered = sum(value[de_name %in%
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
                               )], na.rm = TRUE),
    administered_lt1 = sum(value[metric %in% c("Administered", "Issued") & coc_name %in% c("<1 Years")], na.rm = TRUE),
    administered_gt1 = sum(value[metric %in% c("Administered", "Issued") & coc_name %in% c(">1  Years", "12 - 17 Yrs")], na.rm = TRUE),
    immunized = sum(value[metric %in% c("Fully Immunized Child", "Girls Vaccinated")], na.rm = TRUE)
  ) |>
  mutate(
    consumed = (begin_bal + recieved) - end_bal,
    wasted = consumed - administered
  ) |>
  ungroup() |>
  complete(programme, orgUnit, product, period) |>
  as.data.table()


rm(imm_data)


# Calculate the lagged values within groups
imm_transform[, end_bal_lag1 := shift(end_bal), 
             by = .(programme, orgUnit, product)]

# Calculate rolling sum for each group
imm_transform[, consumed_sum_3m := frollsum(consumed, n = 3, na.rm = TRUE), 
             by = .(programme, orgUnit, product)]

# Calculate rolling sum for each group
imm_transform[, consumed_sum_6m := frollsum(consumed, n = 6, na.rm = TRUE), 
             by = .(programme, orgUnit, product)]

# calculate amc @ site
imm_transform[, amc_3m := frollmean(consumed, n = 3, na.rm = TRUE), 
             by = .(programme, orgUnit, product)]



imm_transform <- imm_transform |>
  dtplyr::lazy_dt() |>
  group_by(programme, orgUnit, product, period) |>
  arrange(programme, orgUnit, product) |>
  ungroup() |>
  mutate(diff = end_bal_lag1 - begin_bal,
         abs_diff = abs(diff),
         abs_diff_percent = ifelse(end_bal_lag1 == 0, NA_real_ , abs(abs_diff / end_bal_lag1)),
         abs_diff_percent_10 = dplyr::case_when(abs_diff_percent <= 0.1 ~ 1,
                                                abs_diff_percent > 0.1 ~ 0,
                                                TRUE ~ NA_real_),
         mos_3m = ifelse(amc_3m == 0, NA_real_ , end_bal / amc_3m),
         stock_status = case_when(consumed_sum_6m > 0 & (is.na(end_bal) | end_bal == 0) ~ "Stock out",
                                  mos_3m > 0 & mos_3m < 1 ~ "Understock",
                                  mos_3m >= 1 & mos_3m <= 2 ~ "Adequate",
                                  mos_3m > 2 ~ "Overstock",
                                  TRUE ~ NA_character_),
         expected_end_bal = (rowSums(select(., c("begin_bal", "recieved")), na.rm = TRUE) -
                               rowSums(select(., c("consumed")), na.rm = TRUE))
  ) |>
  group_by(programme, orgUnit, product, period) |>
  ungroup() |>
  mutate(program_id = imm_dataset) |>
  as.data.table() 

# Load to DB
dbWriteTable(con, "immunization", imm_transform, overwrite = TRUE, row.names = FALSE)

# Set the batch size
# batch_size <- 50000
# 
# 
# # Get the number of rows in the data frame
# n <- nrow(imm_transform)
# 
# # Loop through the data frame in chunks
# for (i in seq(1, n, by = batch_size)) {
#   # Determine the start and end of the current chunk
#   chunk <- imm_transform[i:min(i+batch_size-1, n), ]
#   
#   # Use dbWriteTable to write the current chunk
#   dbWriteTable(con, 
#                name = "immunization", 
#                value = chunk, 
#                row.names = FALSE, 
#                append = ifelse(i == 1, FALSE, TRUE))  # Use overwrite=TRUE only for the first batch
# }
# 
# # Disconnect after the operation is complete
dbDisconnect(con)



















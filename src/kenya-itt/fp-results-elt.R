rm(list = ls())
project_path <- "~/projects/insupplyHealth/kenya_itt/data/kenya_itt/"
source(paste0(project_path, "setup.R"))
#-----------------------------------------------------------------------
# important!
categoryoptioncombos <- datimutils::getMetadata("categoryOptionCombos", 
                                                fields = "id,name") |>
  dplyr::arrange(name) |>
  rename(coc_name = name)

fp_dataelements <- fread(paste0(project_path, "metadata/fp_des_clean.csv")) |>
  select(-c(formName)) |>
  rename(de_name = name)

# results
fp_data <- load_program_data("Family Planning", project_path)

fp_data <- fp_data %>%
  dtplyr::lazy_dt() %>%
  select(dataElement:value) %>%
  left_join(fp_dataelements, by = c("dataElement" = "id")) %>%
  left_join(categoryoptioncombos, by = c("categoryOptionCombo" = "id")) %>%
  filter(!product %in% c("delete", "Others")) %>%
  filter(valueType %in% c("NUMBER", "INTEGER", "INTEGER_ZERO_OR_POSITIVE")) %>%
  mutate(
    value = as.numeric(value),
    period = as.numeric(period)
  ) |>
  as.data.frame()

# Columns
fp_transform <- fp_data |>
  dtplyr::lazy_dt() |>
  arrange(programme, orgUnit, product, period) |>
  group_by(programme, orgUnit, product, period) |>
  arrange(orgUnit, product, period) |>
  summarise(
    end_bal = sum(value[coc_name == "Ending Balance"], na.rm = TRUE),
    begin_bal = sum(value[coc_name == "Beginning Balance"]),
    recieved = sum(value[coc_name %in% c("Quantity Received From KEMSA", "Quantity Received From Kenya Red Cross")], na.rm = TRUE),
    dispensed = sum(value[coc_name == "Dispensed"], na.rm = TRUE),
    clients = sum(value[metric == "services"], na.rm = TRUE),
    expired = sum(value[coc_name == "Expired"], na.rm = TRUE),
    expire_in_6m = sum(value[coc_name == "Quantity expiring in less than 6 months"], na.rm = TRUE),
    losses = sum(value[coc_name == "Losses"], na.rm = TRUE),
    negative_adjust = sum(value[coc_name == "Negative"], na.rm = TRUE),
    positive_adjust = sum(value[coc_name == "Positive"], na.rm = TRUE)
  ) |>
  ungroup() |>
  complete(programme, orgUnit, product, period) |>
  as.data.table()

rm(fp_data)


# Calculate the lagged values within groups
fp_transform[, end_bal_lag1 := shift(end_bal), 
             by = .(programme, orgUnit, product)]

# Calculate rolling sum for each group
fp_transform[, dispensed_sum_3m := frollsum(dispensed, n = 3, na.rm = TRUE), 
             by = .(programme, orgUnit, product)]

# Calculate rolling sum for each group
fp_transform[, dispensed_sum_6m := frollsum(dispensed, n = 6, na.rm = TRUE), 
             by = .(programme, orgUnit, product)]

# calculate amc @ site
fp_transform[, amc_3m := frollmean(dispensed, n = 3, na.rm = TRUE), 
             by = .(programme, orgUnit, product)]



fp_transform <- fp_transform |>
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
         stock_status = case_when(dispensed_sum_6m > 0 & (is.na(end_bal) | end_bal == 0) ~ "Stock out",
                                  mos_3m > 0 & mos_3m < 2 ~ "Understock",
                                  mos_3m >= 2 & mos_3m <= 4 ~ "Adequate",
                                  mos_3m > 4 ~ "Overstock",
                                  TRUE ~ NA_character_),
         expected_end_bal = (rowSums(select(., c("begin_bal", "recieved", "positive_adjust")), na.rm = TRUE) -
                               rowSums(select(., c("dispensed", "losses", "negative_adjust")), na.rm = TRUE))
         ) |>
  group_by(programme, orgUnit, product, period) |>
  ungroup() |>
  mutate(program_id = fp_dataset) |>
  as.data.table() 

dmpa <- fp_transform |> filter(product == "DMPA-SC") |> filter(orgUnit == "A2m3Fgwhf2v")

dbWriteTable(con, "family_planning", fp_transform, overwrite = TRUE, row.names = FALSE)
#rm(list = ls())



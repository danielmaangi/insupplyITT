rm(list = ls())
project_path <- "~/projects/insupplyHealth/kenya_itt/data/kenya_itt/"
source(paste0(project_path, "setup.R"))
#-----------------------------------------------------------------------
# important!
categoryoptioncombos <- datimutils::getMetadata("categoryOptionCombos", 
                                                fields = "id,name") |>
  dplyr::arrange(name) |>
  rename(coc_name = name)

mal_dataelements <- fread(paste0(project_path, "metadata/mal_des_clean.csv")) |>
  select(-c(formName)) |>
  rename(de_name = name)

# results
mal_data <- load_program_data("Malaria", project_path)

mal_data <- mal_data %>%
  dtplyr::lazy_dt() %>%
  select(dataElement:value) %>%
  left_join(mal_dataelements, by = c("dataElement" = "id")) %>%
  left_join(categoryoptioncombos, by = c("categoryOptionCombo" = "id")) %>%
  filter(!product %in% c("delete", "Others")) %>%
  filter(valueType %in% c("NUMBER", "INTEGER", "INTEGER_ZERO_OR_POSITIVE")) %>%
  mutate(
    value = as.numeric(value),
    period = as.numeric(period)
  ) |>
  as.data.frame()

# Columns
mal_transform <- mal_data |>
  dtplyr::lazy_dt() |>
  arrange(programme, orgUnit, product, period) |>
  group_by(programme, orgUnit, product, period) |>
  arrange(orgUnit, product, period) |>
  summarise(
    end_bal = sum(value[coc_name == "Physical Count"], na.rm = TRUE),
    begin_bal = sum(value[coc_name == "Beginning Balance."]),
    recieved = sum(value[coc_name %in% c("Quantity Received this period.")], na.rm = TRUE),
    dispensed = sum(value[coc_name == "Total Quantity dispensed."], na.rm = TRUE),
    # clients = sum(value[coc_name == "New clients"]) + sum(value[coc_name == "Re-visits"], na.rm = TRUE),
    expired = sum(value[coc_name == "Quantity of Expired Drugs."], na.rm = TRUE),
    expire_in_6m = sum(value[coc_name == "Medicines with 6 months to Expiry."], na.rm = TRUE),
    losses = sum(value[coc_name == "Losses (Excluding Expiries)."], na.rm = TRUE),
    negative_adjust = sum(value[coc_name == "Negative Adjustments."], na.rm = TRUE),
    positive_adjust = sum(value[coc_name == "Postive Adjustments."], na.rm = TRUE)
  ) |>
  ungroup() |>
  complete(programme, orgUnit, product, period) |>
  as.data.table()

rm(mal_data)


# Calculate the lagged values within groups
mal_transform[, end_bal_lag1 := shift(end_bal), 
             by = .(programme, orgUnit, product)]

# Calculate rolling sum for each group
mal_transform[, dispensed_sum_3m := frollsum(dispensed, n = 3, na.rm = TRUE), 
             by = .(programme, orgUnit, product)]

# Calculate rolling sum for each group
mal_transform[, dispensed_sum_6m := frollsum(dispensed, n = 6, na.rm = TRUE), 
             by = .(programme, orgUnit, product)]

# calculate amc @ site
mal_transform[, amc_3m := frollmean(dispensed, n = 3, na.rm = TRUE), 
             by = .(programme, orgUnit, product)]



mal_transform <- mal_transform |>
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
  mutate(program_id = mal_dataset) |>
  as.data.table() 

AL12 <- mal_transform |> filter(product == "AL12") |> filter(orgUnit == "A2m3Fgwhf2v")

dbWriteTable(con, "malaria", mal_transform, overwrite = TRUE, row.names = FALSE)
#rm(list = ls())



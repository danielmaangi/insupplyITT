#----------------------------------------------------------------------------------------------
# # read and combine data previous year data
#---------------------------------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(data.table)

dataElements <- fread("metadata/HPT_DES.csv")

# HPT Data
#--------------------------------------------------------------------
hpt <- list.files(
  path = "G:/.shortcut-targets-by-id/1-I-AM6fIfll60idw-jnzME08IVKzNdfj/MLE_DATA/Dashboards/Production/HPT/dataModel/DHIS2/hiskenya/raw/HPT",
  pattern = "*.csv", full.names = T) %>%
  map(read_csv, col_types = cols(.default = "c")) %>%
  map_df(rbind)
glimpse(hpt)

hpt2 <- hpt %>%
  left_join(dataElements, by = c("dataElement" = "id")) %>%
  filter(valueType == "INTEGER_ZERO_OR_POSITIVE") %>%
  mutate(DateValues = NA_Date_,
         NumberValues = value
  ) %>%
  select(- value)

fwrite(hpt2, "G:/.shortcut-targets-by-id/1-I-AM6fIfll60idw-jnzME08IVKzNdfj/MLE_DATA/Dashboards/Production/HPT/dataModel/DHIS2/hiskenya/Download/HPT.csv", na = "")

# HPT Reports
#--------------------------------------------------------------------
hpt_reports_dataset <- list.files(
  path = "G:/.shortcut-targets-by-id/1-I-AM6fIfll60idw-jnzME08IVKzNdfj/MLE_DATA/Dashboards/Production/HPT/dataModel/DHIS2/hiskenya/raw/REPORTS",
  pattern = "*.csv", full.names = T) %>%
  map(read_csv, col_types = cols(.default = "c")) %>%
  map_df(rbind) 
glimpse(hpt_reports_dataset)

fwrite(hpt_reports_dataset, "G:/.shortcut-targets-by-id/1-I-AM6fIfll60idw-jnzME08IVKzNdfj/MLE_DATA/Dashboards/Production/HPT/dataModel/DHIS2/hiskenya/Download/HPT_REPORTS.csv", na = "")

















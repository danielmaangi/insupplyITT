#----------------------------------------------------------------------------------------------
# # read and combine data previous year data
#---------------------------------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(data.table)

dataElements <- fread("data/kenya-hpt/metadata/HPT_DES.csv")

# HPT Data
#--------------------------------------------------------------------
hpt <- list.files(
  path = "data/kenya-hpt/raw/results/",
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

fwrite(hpt2, "data/kenya-hpt/clean/HPT.csv", na = "")

# HPT Reports
#--------------------------------------------------------------------
hpt_reports_dataset <- list.files(
  path = "data/kenya-hpt/raw/reports/",
  pattern = "*.csv", full.names = T) %>%
  map(read_csv, col_types = cols(.default = "c")) %>%
  map_df(rbind) 
glimpse(hpt_reports_dataset)

fwrite(hpt_reports_dataset, "data/kenya-hpt/clean/REPORTS.csv", na = "")

















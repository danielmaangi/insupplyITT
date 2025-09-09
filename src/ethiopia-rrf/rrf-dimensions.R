# Metadata
library(readxl)
library(tidyverse)
library(data.table)

products <- read_excel("data/ethiopia-rrf/metadata/RRF Template.xlsx",
                       sheet = "Bote",
                       skip = 3) 

colnames(products)[1] <- "serial_no"
colnames(products)[2] <- "programme"
colnames(products)[3] <- "product"

fwrite(products,"data/ethiopia-itt/clean/products.csv")


facilities <- read_excel("data/ethiopia-itt/metadata/ET Tracer Commodities.xlsx",
                       sheet = "Sheet2")

clean_sites <- facilities %>%
  mutate(
    site_code = paste(Region, Woreda, gsub(" ", "", `Facility Name`) , sep = "-")
  ) %>%
  mutate(
    site_code = str_replace_all(str_to_lower(site_code)," ", "")
  )

fwrite(clean_sites,"data/ethiopia-itt/clean/sites.csv")


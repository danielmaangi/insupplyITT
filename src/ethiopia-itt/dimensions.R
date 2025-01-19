# Metadata
library(readxl)
library(tidyverse)
library(data.table)

products <- read_excel("ET Tracer Commodities.xlsx",
                       sheet = "Template",
                       skip = 12) |>
  select(-c(`Stock on Hand`, `Total consumed quantity in the month`, `No. of Days out of stock in the month`))

colnames(products)[1] <- "serial_no"
colnames(products)[2] <- "programme"
colnames(products)[3] <- "product"

fwrite(products,"products.csv")


facilities <- read_excel("ET Tracer Commodities.xlsx",
                       sheet = "Sheet2")

clean_sites <- facilities %>%
  mutate(
    site_code = paste(Region, Woreda, gsub(" ", "", `Facility Name`) , sep = "-")
  ) %>%
  mutate(
    site_code = str_to_lower(site_code)
  )

fwrite(clean_sites,"sites.csv")

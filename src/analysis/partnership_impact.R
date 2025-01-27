



library(DBI)
library(RMySQL)  # or RMariaDB
library(dplyr)
library(tidyr)

# Replace with your database credentials
con <- dbConnect(
  MySQL(),
  dbname = "ish_kenya_itt",
  host = "158.220.96.127",  # or your server address
  port = 3306,         # default MySQL port
  user = "admin",
  password = "Admin@2024"
)


# Pull data from the 'family_planning' table
fp_data <- dbGetQuery(con, paste(
  "SELECT programme, orgUnit, product, period, stock_status",
  "FROM family_planning"
))

glimpse(fp_data)
# Pull data from the 'orgunits' table
orgunits <- dbGetQuery(con, "SELECT * FROM orgunits")

glimpse(orgunits)


fp_sor <- fp_data %>%
  left_join(orgunits, by = c("orgUnit" = "id")) %>%
  group_by(county, product, period) %>%
  summarise(
    total_sites = sum(!is.na(stock_status)),
    stocked_out = sum(stock_status == "Stock out", na.rm = T)
  ) %>%
  mutate(
    sor = stocked_out / total_sites
  ) %>%
  select(-total_sites, -stocked_out) 











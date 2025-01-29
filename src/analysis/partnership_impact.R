library(DBI)
library(RMySQL)  # or RMariaDB
library(dplyr)
library(tidyr)

# Impact Counties
impact_counties <- c("Nairobi", "Nakuru", "Trans Nzoia", "Kakamega", "Isiolo")


# Replace with your database credentials
source("config/kenya-hpt/hpt_credentials.R")
con <- dbConnect(
  MySQL(),
  dbname = dbname,
  host = host,  # or your server address
  port = port,         # default MySQL port
  user = user,
  password = password
)

# Pull data from the 'orgunits' table
orgunits <- dbGetQuery(con, "SELECT * FROM orgunits")

glimpse(orgunits)


# Pull data from the 'family_planning' table
fp_data <- dbGetQuery(con, paste(
  "SELECT programme, orgUnit, product, period, stock_status",
  "FROM family_planning"
))

glimpse(fp_data)


# Pull data from the 'nutrition' table
nutrition <- dbGetQuery(con, paste(
  "SELECT programme, orgUnit, product, period, stock_status",
  "FROM nutrition"
))

glimpse(nutrition)




library(lubridate)

sor <- bind_rows(fp_data, nutrition)  %>%
  left_join(orgunits, by = c("orgUnit" = "id")) %>%
  mutate(supported = case_when(county %in% impact_counties ~ "Yes",
                               TRUE ~ "No")) %>%
  group_by(programme, county, product, period, supported) %>%
  summarise(
    total_sites = sum(!is.na(stock_status)),
    stocked_out = sum(stock_status == "Stock out", na.rm = T)
  ) %>%
  mutate(
    sor = stocked_out / total_sites
  ) %>%
  #select(-total_sites, -stocked_out) %>%
  ungroup() %>%
  mutate(
    # Convert to Date and format as "Mar-22"
    month_formatted = format(ymd(paste0(period, "01")), "%b-%y"),
    
    # Extract the quarter and year
    quarter_formatted = paste0("Q", quarter(ymd(paste0(period, "01"))), "-", year(ymd(paste0(period, "01")))),
    
    #Check if the month is the last month of the quarter
    is_last_month_quarter = ifelse(month(ymd(paste0(period, "01"))) %in% c(3, 6, 9, 12), "Yes", "No")
  )



write.csv(sor,
          "data/analysis/impact.csv")










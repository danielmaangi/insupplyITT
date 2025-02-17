library(DBI)
library(RMySQL)  # or RMariaDB
library(dplyr)
library(tidyr)
library(data.table)



# Construct ITT Dataset
#-------------------------------------------------------------------------------
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


# Impact Counties
impact_counties <- c('Nairobi', 'Nakuru', 'Trans Nzoia', 'Kakamega', 'Isiolo')

select_geo <- orgunits %>%
  filter(county %in% impact_counties)

fwrite(select_geo,
       "data/analysis/GEO_IMPACT.csv")

# Commodities
products <- c('CoC pills',
           'Cycle Beads',
           'DMPA-IM',
           'DMPA-SC',
           'EC Pills',
           'Female Condoms',
           'Hormonal IUCD',
           'Implant(2-Rod) 3yr',
           'Implant(2-Rod) 5yr',
           'Implants(1-Rod)',
           'Male Condoms',
           'Non-Hormonal IUCD',
           'Progestin only pills'
)

# Family planning results
#-------------------------------------------------------------
fp_data <- dbGetQuery(con, paste(
  "SELECT *",
  "FROM family_planning AS fp",
  "INNER JOIN orgunits AS org ON fp.orgUnit = org.id"
))

fp_data <- fp_data %>%
  filter(county %in% impact_counties) %>%
  filter(product %in% products) %>%
  select(
    -c(code : county)
  )


glimpse(fp_data)


fwrite(fp_data,
          "data/analysis/fp_impact.csv")


# Nutrition results
#-------------------------------------------------------------
con <- dbConnect(
  MySQL(),
  dbname = dbname,
  host = host,  # or your server address
  port = port,         # default MySQL port
  user = user,
  password = password
)

nut_products <- c(
  "Combined Iron (60mg) Folic Acid (400µg)"
)

nutrition_data %>% distinct(product)

nutrition_data <- dbGetQuery(con, paste(
  "SELECT *",
  "FROM nutrition AS nut",
  "INNER JOIN orgunits AS org ON nut.orgUnit = org.id"
))

nutrition_data <- nutrition_data %>%
  filter(county %in% impact_counties) %>%
  filter(product %in% nut_products) %>%
  select(
    -c(code : county)
  )


glimpse(nutrition_data)



fwrite(nutrition_data,
       "data/analysis/nutrition_impact.csv")




# Reports Dataset
#=========================================================================
# Pull data from the 'family_planning' table, joining with orgunits and filtering
con <- dbConnect(
  MySQL(),
  dbname = dbname,
  host = host,  # or your server address
  port = port,         # default MySQL port
  user = user,
  password = password
)

reports_data <- dbGetQuery(con, paste(
  "SELECT *",
  "FROM reports AS reports",
  "INNER JOIN orgunits AS org ON reports.`Organisation unit` = org.id"
))

reports_data <- reports_data %>%
  filter(county %in% impact_counties) %>%
  select(
    -c(code : county)
  )


glimpse(reports_data)


# HPT Reports ONLY
#=============================================
con <- dbConnect(
  MySQL(),
  dbname = dbname,
  host = host,  # or your server address
  port = port,         # default MySQL port
  user = user,
  password = password
)

hpt_reports <- dbGetQuery(con, paste(
  "SELECT *",
  "FROM hpt_reports AS hpt_reports",
  "INNER JOIN orgunits AS org ON hpt_reports.`Organisation unit` = org.id"
))

hpt_reports <- hpt_reports %>%
  filter(county %in% impact_counties) %>%
  separate_wider_delim(Data, ".", names = c("Dataset ID", "Indicator")) |>
  pivot_wider(names_from = Indicator,
              values_from = Value) |>
  filter(EXPECTED_REPORTS > 0) %>%
  mutate(
    Programme = "HPT",
    Period = as.numeric(Period),
    ACTUAL_REPORTS = as.numeric(ACTUAL_REPORTS),
    ACTUAL_REPORTS_ON_TIME = as.numeric(ACTUAL_REPORTS_ON_TIME),
    EXPECTED_REPORTS = as.numeric(EXPECTED_REPORTS)
  ) %>%
    select(
      -c(code : county)
    ) 


glimpse(hpt_reports)

all_reports <- bind_rows(reports_data, hpt_reports)


fwrite(all_reports,
       "data/analysis/reporting_impact.csv")




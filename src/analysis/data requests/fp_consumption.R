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


# Family planning results
#-------------------------------------------------------------
fp_data <- dbGetQuery(con, paste(
  "SELECT *",
  "FROM family_planning AS fp",
  "INNER JOIN orgunits AS org ON fp.orgUnit = org.id"
))

consumption_data <- fp_data %>%
  filter(period > 202301) %>%
  select(orgUnit, product, period, end_bal, begin_bal, recieved, dispensed)

glimpse(consumption_data)


fwrite(consumption_data,
       "data/analysis/fp_data_request.csv")

# Additional dependancies
library(jsonlite)
library(DBI)
library(RMySQL)
library(RPostgreSQL)

# datasets
datasets <- fread(paste0(project_path, "metadata/datasets.csv"))
glimpse(datasets)


# organisation units
orgunits <- fread(paste0(project_path, "metadata/orgunits.csv"))
glimpse(orgunits)

all_reports_data <- fread(paste0(project_path, "transform/reports.csv")) 

glimpse(all_reports_data)

# Connect to the PostgreSQL database
pos_con <- dbConnect(RPostgreSQL::PostgreSQL(),
                 dbname = "shujaaz",
                 host = "localhost",
                 port = 5432,
                 user = "postgres",
                 password = "mMyPa55w0rdd")


# Write or overwrite the dataframe to PostgreSQL
dbWriteTable(pos_con, "datasets", datasets, overwrite = TRUE)

dbWriteTable(pos_con, "orgunits", orgunits, overwrite = TRUE)

dbWriteTable(pos_con, "reports", all_reports_data, overwrite = TRUE)

# Disconnect from the database
dbDisconnect(pos_con)

















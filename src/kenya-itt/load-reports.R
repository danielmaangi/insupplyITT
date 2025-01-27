# This script is for loading data to the datawarehouse
#-----------------------------------------------------------------------
source(paste0(project_path,"R/functions.R"))
login(username, password, base.url)
#--------------------------------------------------------------------
# Additional dependancies
library(jsonlite)
library(DBI)
library(RMySQL)

# datasets
datasets <- fread(paste0(project_path, "metadata/datasets.csv"))
glimpse(datasets)


# organisation units
orgunits <- fread(paste0(project_path, "metadata/orgunits.csv"))
glimpse(orgunits)


# Reports
all_reports_data <- fread(paste0(project_path, "transform/reports.csv")) 

programs <- fread(paste0(project_path,"transform/programs.csv"))

# Connect to MySQL database
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "ish_kenya_itt",
                 host = "localhost",
                 port = 3306,
                 user = "admin",
                 password = "Admin@2024",
                 local_infile = TRUE)

# Create a table and write data in MySQL

# datasets
dbWriteTable(con, "datasets", datasets, overwrite = TRUE)

# organisational units
dbWriteTable(con, "orgunits", orgunits, overwrite = TRUE)

# Reports
dbWriteTable(con, "reports", all_reports_data, overwrite = TRUE)

# Reports
dbWriteTable(con, "programs", programs, overwrite = TRUE)


# Close the database connection
dbDisconnect(con)





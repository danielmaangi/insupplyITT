# This script is for loading data to the datawarehouse
#-----------------------------------------------------------------------
source(paste0(project_path,"R/functions.R"))
login(username, password, base.url)
#--------------------------------------------------------------------
# Additional dependancies
library(jsonlite)
library(DBI)
library(RMySQL)


fp_transform_nulls <- fread(paste0(project_path, "transform/FP/fp_transform_nulls.csv"))
glimpse(fp_transform_nulls)

clean_data <- fp_transform_nulls |>
  mutate(period = as.character(period)) |>
  mutate(sum = rowSums(across(where(is.numeric)))) |>
  filter(sum > 0) |>
  select(-sum) |>
  mutate(period = as.numeric(period))
  

# Connect to MySQL database
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "ish_kenya_itt",
                 host = "localhost",
                 port = 3306,
                 user = "admin",
                 password = "Admin@2024",
                 local_infile = TRUE)


# Reports
dbWriteTable(con, "program_data", clean_data, overwrite = TRUE)


# Close the database connection
dbDisconnect(con)





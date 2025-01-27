# Load the necessary libraries and ensure they are installed
required_packages <- c(
  "httr", "rjson", "tidyverse", "lubridate", "data.table", 
  "readxl", "jsonlite", "datimutils", "RcppRoll", "zoo", 
  "aws.s3", "DBI", "RMySQL", "paws", "future.apply", "sparklyr"
)

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "http://cran.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

#spark_install(version = "3.4.3")

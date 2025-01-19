# install.packages("fs")

# Load the fs package
library(fs)

# Define the source and destination directories
source_dir <- "data/kenya-hpt/"
destination_dir <- "G:/My Drive/Personal/MLE_DATA/Dashboards/HPT/"


library(R.utils)
copyDirectory(from = source_dir, 
              to = destination_dir, 
              recursive = TRUE)

dir_delete(paste0(destination_dir, "/raw/"))

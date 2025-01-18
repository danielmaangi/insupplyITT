# Login function
login <- function(username, password, base.url) {
  url <- paste0(base.url, "api/me")
  r <- GET(url, authenticate(username, password))
  
  if (r$status == 200L) {
    print("Logged in successfully!")
  } else {
    print("Could not login")
  }
}

project_path <- "G:/.shortcut-targets-by-id/1-I-AM6fIfll60idw-jnzME08IVKzNdfj/MLE_DATA/Dashboards/Production/HPT/dataModel/DHIS2/hiskenya/"
datimutils::loginToDATIM(username = username,
             password = password,
             base_url = base.url)



# Function to download data from DHIS2
# Parameters:
# - dataset: The ID of the dataset we want to get data from (e.g., "apples_dataset")
# - startdate: The start date for the data (e.g., "2024-01-01")
# - enddate: The end date for the data (e.g., "2024-03-31")
# - orgunit: The organization unit ID specifying where we want data from (e.g., "org_unit_A")
# Returns:
# - A data frame with the downloaded data, or NULL if there's an error


monthly_download <- function(dataset, startdate, enddate, orgunit, output_folder) {
  # Convert startdate and enddate to Date objects
  start_date <- floor_date(as.Date(startdate), unit = "month")
  end_date <- ceiling_date(as.Date(enddate), unit = "month") - days(1)
  
  # Loop through each month between start_date and end_date
  while (start_date <= end_date) {
    # Calculate the last day of the current month
    last_day <- as.Date(paste0(format(start_date, "%Y-%m"), "-01")) + months(1) - 1
    
    # Construct the URL for the API request for the current month
    url <- paste0(base.url,
                  "api/dataValueSets?dataSet=", dataset,
                  "&startDate=", format(start_date, "%Y-%m-%d"),
                  "&endDate=", format(last_day, "%Y-%m-%d"),
                  "&orgUnit=", orgunit, "&children=true")
    
    # Try sending the request and handling any errors
    tryCatch({
      # Send the GET request to the DHIS2 API
      r <- GET(url)
      
      # Check if the response is in JSON format
      if (http_type(r) == "application/json") {
        # Parse the JSON content
        content <- content(r, as = "parsed")
        
        # Check if the response contains dataValues
        if ("dataValues" %in% names(content) && length(content$dataValues) > 0) {
          # Convert dataValues to a data frame
          data <- rbindlist(content$dataValues, fill = TRUE) |>
            mutate(extract_date = lubridate::now())
          
          # Save data to a file in the output_folder
          filename <- file.path(output_folder, paste0("data_", format(start_date, "%Y-%m"), ".csv"))
          write.csv(data, filename, row.names = FALSE)
        } else {
          message("No dataValues found in the API response for month ", format(start_date, "%Y-%m"))
        }
      } else {
        message("Unexpected content type received from the API for month ", format(start_date, "%Y-%m"))
      }
    }, error = function(e) {
      message("Error in API request for month ", format(start_date, "%Y-%m"), ":", e$message)
    })
    
    # Move to the next month
    start_date <- start_date + months(1)
  }
}




fetch_reports <- function(dataset, startdate, enddate, orgunit, output_folder) {
  # Convert startdate and enddate to Date objects
  start_date <- floor_date(as.Date(startdate), unit = "month")
  end_date <- ceiling_date(as.Date(enddate), unit = "month") - days(1)
  
  # Loop through each month between start_date and end_date
  while (start_date <= end_date) {
    # Calculate the last day of the current month
    month <- format(as.Date(start_date), "%Y%m")
    
    # Return the data
    data <- datimutils::getAnalytics("displayProperty=NAME", "hierarchyMeta=true",
                         dx = c(paste(dataset, "EXPECTED_REPORTS", sep = "."),
                                paste(dataset, "ACTUAL_REPORTS", sep = "."),
                                paste(dataset, "ACTUAL_REPORTS_ON_TIME", sep = ".")
                         ),
                         pe = month,
                         ou = c("HfVjCurKxh2", "LEVEL-5"),
                         timeout = 300)|>
      mutate(extract_date = lubridate::now())
    
    # Save data to a file in the output_folder
    filename <- file.path(output_folder, paste0("data_", format(start_date, "%Y-%m"), ".csv"))
    write.csv(data, filename, row.names = FALSE)
    
    # Move to the next month
    start_date <- start_date + months(1)
  }
}



load_program_data <- function(program_name, project_path) {
  # Construct the file path based on the program name
  file_path <- paste0(project_path, "raw/", program_name, "/")
  
  # Load all CSV files from the specified directory and add a 'Programme' column
  program_data <- map_df(list.files(file_path, pattern = "\\.csv$", full.names = TRUE),
                         ~fread(.x, colClasses = "character")) %>%
    mutate(Programme = program_name)
  
  return(program_data)
}


login(username, password, base.url)
monthly_download("ppdqEN91qZs", 
                 startdate, enddate, 
                 orgunit, 
                 paste0(project_path, "raw/HPT/"))


# Reporting Rates
login(username, password, base.url)
fetch_reports("ppdqEN91qZs", 
              startdate, enddate, 
              orgunit, 
              paste0(project_path, "raw/REPORTS/"))


# fetch_csv <- function(bucket, folder) {
#   library(paws)
#   library(data.table)
#   
#   # List objects in the specified folder
#   objects <- s3$list_objects_v2(Bucket = bucket, Prefix = folder)
#   
#   # Check if there are any contents
#   if (length(objects$Contents) == 0) {
#     stop("No files found in the specified folder.")
#   }
#   
#   # Filter CSV files
#   csv_files <- sapply(objects$Contents, function(x) x$Key)
#   csv_files <- csv_files[grepl("\\.csv$", csv_files)]
#   
#   # Initialize an empty list to store data frames
#   data_list <- list()
#   
#   # Read each CSV file and store it in the list
#   for (file_key in csv_files) {
#     obj <- s3$get_object(Bucket = bucket, Key = file_key)
#     raw_data <- rawToChar(obj$Body)
#     data <- fread(raw_data)
#     data_list[[file_key]] <- data
#   }
#   
#   # Combine all data frames into one
#   combined_data <- rbindlist(data_list, use.names = TRUE, fill = TRUE)
#   
#   return(combined_data)
# }








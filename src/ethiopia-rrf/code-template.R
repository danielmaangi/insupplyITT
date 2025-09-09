# Excel RRF (Report and Requisition Form) Reader and Cleaner
# This script reads Excel workbooks with multiple sheets and generates clean data frames

# Required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# Function to read and clean RRF Excel workbook
read_rrf_workbook <- function(file_path, skip_sheets = c("Deleted", "Sheet1")) {
  
  # Get all sheet names
  sheet_names <- excel_sheets(file_path)
  
  # Filter out unwanted sheets
  data_sheets <- sheet_names[!sheet_names %in% skip_sheets]
  
  cat("Found", length(data_sheets), "data sheets:", paste(data_sheets, collapse = ", "), "\n")
  
  # Function to clean a single sheet
  clean_single_sheet <- function(sheet_name) {
    cat("Processing sheet:", sheet_name, "\n")
    
    # Read the raw data
    raw_data <- read_excel(file_path, sheet = sheet_name, col_names = FALSE)
    
    # Find the header rows (looking for "Ser. No." which indicates start of data table)
    header_row <- which(str_detect(raw_data[[1]], "Ser\\. No\\.|Serial No\\."))
    
    if (length(header_row) == 0) {
      warning("Could not find header row in sheet: ", sheet_name)
      return(NULL)
    }
    
    header_row <- header_row[1]  # Take first occurrence
    
    # Create proper column names by combining rows
    col_names <- character(ncol(raw_data))
    
    # Primary headers (row with "Ser. No.")
    primary_headers <- as.character(raw_data[header_row, ])
    
    # Secondary headers (next row, usually contains "Beginning Balance", etc.)
    if (header_row + 1 <= nrow(raw_data)) {
      secondary_headers <- as.character(raw_data[header_row + 1, ])
    } else {
      secondary_headers <- rep("", ncol(raw_data))
    }
    
    # Combine headers intelligently
    for (i in 1:length(col_names)) {
      primary <- if_else(is.na(primary_headers[i]) | primary_headers[i] == "", "", primary_headers[i])
      secondary <- if_else(is.na(secondary_headers[i]) | secondary_headers[i] == "", "", secondary_headers[i])
      
      if (primary != "" && secondary != "") {
        col_names[i] <- paste(primary, secondary, sep = "_")
      } else if (primary != "") {
        col_names[i] <- primary
      } else if (secondary != "") {
        col_names[i] <- secondary
      } else {
        col_names[i] <- paste0("Col_", i)
      }
    }
    
    # Clean column names
    col_names <- str_replace_all(col_names, "[^A-Za-z0-9_]", "_")
    col_names <- str_replace_all(col_names, "_+", "_")
    col_names <- str_remove_all(col_names, "^_|_$")
    col_names <- make.names(col_names, unique = TRUE)
    
    # Read data starting from after headers
    data_start_row <- header_row + 2  # Skip the combined header rows
    
    # Find where data actually starts (first row with a serial number)
    for (i in data_start_row:nrow(raw_data)) {
      if (!is.na(raw_data[i, 1]) && is.numeric(as.numeric(raw_data[i, 1]))) {
        data_start_row <- i
        break
      }
    }
    
    # Read the actual data
    data <- read_excel(
      file_path, 
      sheet = sheet_name, 
      skip = data_start_row - 1,
      col_names = col_names[1:min(length(col_names), 14)]  # Limit to reasonable number of columns
    )
    
    # Clean the data
    data_clean <- data %>%
      # Remove completely empty rows
      filter(!if_all(everything(), ~ is.na(.x) | .x == "")) %>%
      # Remove rows that are section headers (no serial number)
      filter(!is.na(get(names(.)[1])) & get(names(.)[1]) != "") %>%
      # Convert serial numbers to numeric where possible
      mutate(
        across(1, ~ as.numeric(.x)),
        # Add sheet identifier
        sheet_name = sheet_name,
        # Add period information if sheet name looks like a date
        period = case_when(
          str_detect(sheet_name, "^\\d{6}$") ~ paste0(str_sub(sheet_name, 1, 4), "-", str_sub(sheet_name, 5, 6)),
          TRUE ~ sheet_name
        )
      ) %>%
      # Reorder columns to put identifiers first
      select(sheet_name, period, everything()) %>%
      # Remove rows where serial number is not numeric (these are likely section headers)
      filter(!is.na(get(names(.)[3])))
    
    return(data_clean)
  }
  
  # Process all sheets
  all_data <- map_dfr(data_sheets, possibly(clean_single_sheet, otherwise = NULL))
  
  return(all_data)
}

# Function to extract facility information from the first sheet
extract_facility_info <- function(file_path) {
  
  # Read first few rows to extract facility information
  header_data <- read_excel(file_path, sheet = 1, range = "A1:N5", col_names = FALSE)
  
  facility_info <- list()
  
  # Extract facility name
  facility_row <- str_which(header_data[[1]], "Name of Health Facility")
  if (length(facility_row) > 0) {
    facility_info$facility_name <- as.character(header_data[facility_row, 2])
  }
  
  # Extract region, zone, woreda
  location_row <- str_which(header_data[[1]], "Region:|Zone:|Woreda:")
  if (length(location_row) > 0) {
    row_data <- as.character(header_data[location_row[1], ])
    
    # Extract region
    region_col <- str_which(row_data, "Region:")
    if (length(region_col) > 0 && region_col < length(row_data)) {
      facility_info$region <- str_trim(row_data[region_col + 1])
    }
    
    # Extract zone
    zone_col <- str_which(row_data, "Zone:")
    if (length(zone_col) > 0 && zone_col < length(row_data)) {
      facility_info$zone <- str_trim(row_data[zone_col + 1])
    }
    
    # Extract woreda
    woreda_col <- str_which(row_data, "Woreda:")
    if (length(woreda_col) > 0 && woreda_col < length(row_data)) {
      facility_info$woreda <- str_trim(row_data[woreda_col + 1])
    }
  }
  
  # Extract EPSS branch
  epss_row <- str_which(header_data[[1]], "Supplying EPSS Branch")
  if (length(epss_row) > 0) {
    facility_info$epss_branch <- as.character(header_data[epss_row, 2])
  }
  
  return(facility_info)
}

# Main function to process RRF workbook
process_rrf_workbook <- function(file_path, include_facility_info = TRUE) {
  
  cat("Reading RRF workbook:", file_path, "\n")
  
  # Extract facility information
  if (include_facility_info) {
    facility_info <- extract_facility_info(file_path)
    cat("Facility:", facility_info$facility_name %||% "Not specified", "\n")
    cat("Region:", facility_info$region %||% "Not specified", "\n")
  }
  
  # Read and clean all data sheets
  clean_data <- read_rrf_workbook(file_path)
  
  # Add facility information to the data if requested
  if (include_facility_info && nrow(clean_data) > 0) {
    for (info_name in names(facility_info)) {
      clean_data[[info_name]] <- facility_info[[info_name]]
    }
  }
  
  cat("Successfully processed", nrow(clean_data), "records from", 
      length(unique(clean_data$sheet_name)), "sheets\n")
  
  return(clean_data)
}

# Example usage:
# Replace 'your_file.xlsx' with the actual file path
file_path <- "data\\ethiopia-rrf\\RRF Latest Template.xlsx"

# Process the workbook
# rrf_data <- process_rrf_workbook(file_path)

# View the structure
# str(rrf_data)
# head(rrf_data)

# Additional utility functions

# Function to analyze data completeness
analyze_completeness <- function(data) {
  
  # Count missing values by column
  missing_summary <- data %>%
    summarise(across(everything(), ~ sum(is.na(.x) | .x == ""))) %>%
    pivot_longer(everything(), names_to = "column", values_to = "missing_count") %>%
    mutate(
      total_rows = nrow(data),
      missing_percentage = round(missing_count / total_rows * 100, 2)
    ) %>%
    arrange(desc(missing_percentage))
  
  return(missing_summary)
}

# Function to get summary statistics for numeric columns
get_numeric_summary <- function(data) {
  
  numeric_cols <- data %>%
    select(where(is.numeric)) %>%
    names()
  
  if (length(numeric_cols) > 0) {
    summary_stats <- data %>%
      select(all_of(numeric_cols)) %>%
      pivot_longer(everything(), names_to = "column", values_to = "value") %>%
      group_by(column) %>%
      summarise(
        count = sum(!is.na(value)),
        mean = round(mean(value, na.rm = TRUE), 2),
        median = round(median(value, na.rm = TRUE), 2),
        min = min(value, na.rm = TRUE),
        max = max(value, na.rm = TRUE),
        .groups = 'drop'
      )
    
    return(summary_stats)
  } else {
    return(data.frame(message = "No numeric columns found"))
  }
}

cat("RRF Excel reader functions loaded successfully!\n")
cat("Use process_rrf_workbook('your_file.xlsx') to read and clean your data.\n")


process_data <- process_rrf_workbook(file_path)
















# Load the Excel workbook
excel_file <- readxl::read_excel(thepath, 
                                 sheet = NULL)

# Get the sheet names
sheet_names <- readxl::excel_sheets(thepath)
sheet_names

# Create an empty list to store the data frames
data_list <- list()

# Loop through the sheet names and read the data into separate data frames
for (i in 1:length(sheet_names)) {
  data_list[[i]] <- readxl::read_excel(thepath, 
                                       sheet = sheet_names[i])
  data_list[[i]]$MonthKey <- sheet_names[i]
}

data_list

# Combine the data frames into a single data frame
all_data <- do.call(rbind, data_list) |>
  mutate(Month = as.Date(Month)) |>
  mutate(across(c(`Total patients` : Stopped), ~ replace(., . %in% c("-", " "), NA)))

glimpse(all_data)





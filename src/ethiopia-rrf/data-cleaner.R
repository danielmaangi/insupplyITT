# Install and load required libraries
# install.packages(c("googledrive", "readxl", "dplyr", "data.table", "tidyverse", "purrr", "glue", "zoo", "testthat"))
library(googledrive)
library(readxl)
library(data.table)
library(tidyverse)
library(purrr)
library(glue)
library(zoo)
library(testthat)

# Load utility functions (assuming these exist)
# source("src/ethiopia-itt/utils.R")

# Test function for data validation
test_missing <- function(data, output_csv_path) {
  test_that("Check for missing values", {
    # Check if required columns exist
    expect_true("site_code" %in% colnames(data), "The 'site_code' column is missing from the dataset.")
    expect_true("serial_no" %in% colnames(data), "The 'serial_no' column is missing from the dataset.")
    
    # Check for missing values in critical columns
    site_missing_count <- sum(is.na(data$site_code))
    item_code <- sum(is.na(data$serial_no))
    
    expect_equal(site_missing_count, 0, info = glue("There are {site_missing_count} missing values in 'site_code'."))
    expect_equal(item_code, 0, info = glue("There are {item_code} missing values in 'serial_no'."))
    
    # Save data if validation passes
    if (site_missing_count == 0 && item_code == 0) {
      fwrite(data, output_csv_path)
      cat(glue("Data saved to {output_csv_path}\n"))
    } 
  })
}

# Function to extract files from Google Drive
extract_files <- function(subfolders, data_dir) {
  for (i in seq_len(nrow(subfolders))) {
    folder_name <- subfolders$name[i]
    folder_id <- subfolders$id[i]
    
    # List files in the current subfolder
    files <- drive_ls(as_id(folder_id)) %>%
      filter(grepl("\\.xlsx$", name) | grepl("\\.xls$", name))
    
    # Download Excel files locally
    lapply(seq_len(nrow(files)), function(j) {
      drive_download(as_id(files$id[j]), 
                     path = file.path(data_dir, files$name[j]),
                     overwrite = TRUE)
    })
  }
}

# Function to read Excel files with improved range detection
read_files <- function(data_dir, subfolders) {
  all_data <- list()
  
  for (i in seq_len(nrow(subfolders))) {
    folder_name <- subfolders$name[i]
    
    # Get file names in the folder
    files <- list.files(path = data_dir, pattern = "\\.xlsx$|\\.xls$", full.names = TRUE)
    
    # Read all sheets from the downloaded files
    folder_data <- lapply(files, function(file_path) {
      sheets <- excel_sheets(file_path)
      
      # Filter out empty or irrelevant sheets
      valid_sheets <- sheets[!sheets %in% c("Sheet1", "Sheet2")]
      
      lapply(valid_sheets, function(sheet) {
        tryCatch({
          # Read with flexible range - start from row 9 where data typically begins
          read_excel(file_path, 
                     sheet = sheet, 
                     skip = 8,  # Skip header rows
                     col_types = "text",
                     .name_repair = "minimal")
        }, error = function(e) {
          warning(glue("Failed to read sheet {sheet} from {basename(file_path)}: {e$message}"))
          NULL
        })
      })
    })
    names(folder_data) <- basename(files)
    
    # Store data for the current folder
    all_data[[folder_name]] <- folder_data
  }
  
  return(all_data)
}

# Load product reference data
clean_product_list <- tryCatch({
  fread("data/ethiopia-itt/clean/products.csv") %>%
    select(serial_no, product, programme) %>%
    rename(sn = serial_no, prog = programme)
}, error = function(e) {
  warning("Products.csv not found. Creating empty reference.")
  data.table(sn = integer(), product = character(), prog = character())
})

products_copy <- clean_product_list %>%
  rename(prog_copy = prog, item_copy = product)

# Main processing function
process_ethiopia_itt_data <- function() {
  # Authenticate with Google Drive
  tryCatch({
    drive_auth("daniel_maangi@insupplyhealth.com")
  }, error = function(e) {
    warning("Google Drive authentication failed. Please authenticate manually.")
  })
  
  # Get the parent folder containing subfolders for each month
  parent_folder_name <- "https://drive.google.com/drive/folders/1KVm2HCGst-E8XtZJwZ-R70sdzE5dG5rF"
  parent_folder <- drive_get(parent_folder_name)
  
  # List all items in the parent folder
  subfolders <- drive_ls(parent_folder, type = "folder")
  print(subfolders)
  
  # Ensure the data directory exists
  data_dir <- "data/ethiopia-itt/raw/results"
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
  
  # Extract and read files
  extract_files(subfolders, data_dir)
  all_data <- read_files(data_dir, subfolders)
  
  # Combine data from all folders and files
  combined_data <- bind_rows(
    lapply(names(all_data), function(folder_name) {
      folder_data <- all_data[[folder_name]]
      bind_rows(
        lapply(names(folder_data), function(file_name) {
          file_sheets <- folder_data[[file_name]]
          bind_rows(
            lapply(seq_along(file_sheets), function(sheet_idx) {
              sheet_data <- file_sheets[[sheet_idx]]
              if (!is.null(sheet_data) && nrow(sheet_data) > 0) {
                sheet_data$sheet_name <- names(file_sheets)[sheet_idx] %||% paste0("Sheet", sheet_idx)
                sheet_data$file_name <- file_name
                sheet_data$folder_name <- folder_name
                return(sheet_data)
              }
              return(NULL)
            }),
            .id = "sheet_id"
          )
        }),
        .id = "file_id"
      )
    }),
    .id = "folder_id"
  )
  
  # Clean and standardize column names based on the RRF format
  if (ncol(combined_data) >= 10) {
    # Adjust column naming based on actual structure
    col_names <- c("month", "facility", "sheet_name", "serial_no", "item", "unit", 
                   "beginning_balance", "qty_received", "loss_adjustment", "ending_balance_du",
                   "ending_balance_store", "consumption", "days_stocked_out", "max_stock_qty")
    
    # Only rename columns that exist
    for (i in seq_along(col_names)) {
      if (i <= ncol(combined_data)) {
        colnames(combined_data)[i] <- col_names[i]
      }
    }
  }
  
  # Data cleaning and transformation
  clean_data <- combined_data %>%
    # Extract month from folder name or file name
    mutate(
      month = coalesce(folder_name, str_extract(file_name, "\\d{1,2}(?:th|st|nd|rd)?\\s*month")),
      facility = case_when(
        !is.na(facility) ~ facility,
        str_detect(file_name, "HC|Health") ~ str_extract(file_name, "[^_]+(?=_)"),
        TRUE ~ "Unknown"
      )
    ) %>%
    # Convert numeric columns
    mutate(
      across(c(beginning_balance, qty_received, loss_adjustment, ending_balance_du, 
               ending_balance_store, consumption, days_stocked_out, max_stock_qty), 
             ~as.numeric(as.character(.x))),
      serial_no = as.integer(as.character(serial_no))
    ) %>%
    # Calculate stock on hand and consumed values
    mutate(
      stock_on_hand = coalesce(ending_balance_du, ending_balance_store, 0),
      consumed = coalesce(consumption, beginning_balance + qty_received - stock_on_hand, 0),
      days_stocked_out = coalesce(days_stocked_out, 0)
    ) %>%
    # Filter out empty rows
    filter(
      !is.na(serial_no) | !is.na(item),
      !is.na(stock_on_hand) | !is.na(consumed) | !is.na(days_stocked_out)
    ) %>%
    # Calculate adjustment factors
    mutate(
      adjust_factor = case_when(
        is.na(days_stocked_out) | days_stocked_out == 0 ~ 1,
        TRUE ~ 30.5 / (30.5 - days_stocked_out)
      ),
      adjusted_consumption = consumed * adjust_factor
    ) %>%
    # Extract date from month field
    mutate(
      date = case_when(
        str_detect(month, "\\b\\d{4}\\b") ~ dmy(paste0("1 ", month)),
        str_detect(month, "\\d{1,2}") ~ dmy(paste0("1 ", str_extract(month, "\\d{1,2}"), " 2017")),
        TRUE ~ as.Date(NA)
      )
    ) %>%
    # Facility code mapping (enhanced for RRF format)
    mutate(
      site_code = case_when(
        str_detect(str_to_lower(facility), "wosha") ~ "sidama-wondogenet-woshahealthcenter",
        str_detect(str_to_lower(facility), "addis") ~ "diredawa-diredawa-addisketemahealthcenter",
        str_detect(str_to_lower(facility), "biyoawale") ~ "diredawa-diredawa-biyoawalehealthcenter",
        str_detect(str_to_lower(facility), "boren") ~ "diredawa-diredawa-boren(industrymender)healthcenter",
        str_detect(str_to_lower(facility), "dechatu") ~ "diredawa-diredawa-dechatuhealthcenter",
        str_detect(str_to_lower(facility), "diredawa|dd|dire") ~ "diredawa-diredawa-diredawa(nemberwan)healthcenter",
        str_detect(str_to_lower(facility), "gendegerada") ~ "diredawa-diredawa-gendegeradahealthcenter",
        str_detect(str_to_lower(facility), "gendekore") ~ "diredawa-diredawa-gendekorehealthcenter",
        str_detect(str_to_lower(facility), "goro") ~ "diredawa-diredawa-gorohealthcenter",
        str_detect(str_to_lower(facility), "jeldesa|jeldessa") ~ "diredawa-diredawa-jeldesahealthcenter",
        str_detect(str_to_lower(facility), "jelobelina|jelobeline") ~ "diredawa-diredawa-jelobelinahealthcenter",
        str_detect(str_to_lower(facility), "kalecha|kalicha") ~ "diredawa-diredawa-kalechahealthcenter",
        str_detect(str_to_lower(facility), "legeoda|lege oda|laga") ~ "diredawa-diredawa-legeodagudenfetahealthcenter",
        str_detect(str_to_lower(facility), "legehare") ~ "diredawa-diredawa-legeharehealthcenter",
        str_detect(str_to_lower(facility), "melkajebdu|melka jebdu") ~ "diredawa-diredawa-melkajebduhealthcenter",
        str_detect(str_to_lower(facility), "melkaqero|melkakero|melka kero") ~ "diredawa-diredawa-melkaqerohealthcenter",
        str_detect(str_to_lower(facility), "wahil") ~ "diredawa-diredawa-wahilhealthcenter",
        str_detect(str_to_lower(facility), "dilchora") ~ "diredawa-diredawa-dilchorageneralhospital",
        str_detect(str_to_lower(facility), "sabiyan|sabian") ~ "diredawa-diredawa-sabiyangeneralhospital",
        str_detect(str_to_lower(facility), "ahmadimam|ahemdimam|ahmedimam|ahmed imam") ~ "oromia-jarso-ahmadimamhealthcenter",
        str_detect(str_to_lower(facility), "ale health|alle|ale") ~ "oromia-jarso-alehealthcenter",
        str_detect(str_to_lower(facility), "ananomite|ananoo|anano") ~ "oromia-jarso-ananomitehealthcenter",
        str_detect(str_to_lower(facility), "ejersa goro|jarso health") ~ "oromia-jarso-ejersagoro/jarsohealthcenter",
        str_detect(str_to_lower(facility), "kora") ~ "oromia-jarso-koramitehealthcenter",
        TRUE ~ paste0("unknown-", str_to_lower(str_replace_all(facility, "[^a-zA-Z0-9]", "")))
      )
    ) %>%
    # Join with product list
    left_join(clean_product_list, by = c("item" = "product")) %>%
    mutate(
      serial_no = case_when(
        !is.na(serial_no) & is.na(sn) ~ serial_no,
        is.na(serial_no) & !is.na(sn) ~ sn,
        TRUE ~ serial_no
      )
    ) %>%
    # Additional serial number mapping for common items
    mutate(
      serial_no = case_when(
        str_detect(item, "Albendazole.*400mg") ~ 5,
        str_detect(item, "Amoxicillin.*250mg") ~ 6,
        str_detect(item, "Ciprofloxacin") ~ 10,
        str_detect(item, "TTC eye ointment") ~ 19,
        str_detect(item, "Arthmeter|Arthemeter") ~ 25,
        str_detect(item, "Amlodipine") ~ 26,
        str_detect(item, "Furosemide|Frusamide") ~ 27,
        str_detect(item, "Hydralazine|Hydralizine") ~ 28,
        str_detect(item, "Metformin") ~ 29,
        str_detect(item, "Adrenaline") ~ 30,
        str_detect(item, "Metronidazole") ~ 31,
        str_detect(item, "Normal Saline") ~ 33,
        str_detect(item, "Omeprazole") ~ 34,
        str_detect(item, "Tetanus") ~ 35,
        str_detect(item, "TDF.*3TC.*DTG") ~ 17,
        str_detect(item, "RHZE") ~ 24,
        str_detect(item, "Nevirapine") ~ 1,
        str_detect(item, "Tenofovir.*Lamivudine.*Efavirenz") ~ 2,
        str_detect(item, "Zidovudine") ~ 3,
        TRUE ~ serial_no
      )
    ) %>%
    # Remove invalid entries
    filter(
      !is.na(item),
      item != "190",
      !str_detect(item, "^\\s*$")
    ) %>%
    select(-sn) %>%
    arrange(site_code, serial_no, date) %>%
    as.data.table()
  
  # Join with products copy for program information
  clean_data <- clean_data %>%
    left_join(products_copy, by = c("serial_no" = "sn"))
  
  # Calculate rolling consumption metrics
  clean_data[, consumed_in_6m := frollsum(consumed, n = 6, na.rm = TRUE), 
             by = .(site_code, serial_no)]
  
  # Calculate 3-month average monthly consumption
  clean_data[, amc_3m := zoo::rollapply(adjusted_consumption, width = 3, 
                                        FUN = function(x) mean(x, na.rm = TRUE), 
                                        partial = TRUE, align = "right"), 
             by = .(site_code, serial_no)]
  
  # SATP (Stock Adequacy and Treatment Performance) Calculations
  clean_data <- clean_data %>%
    mutate(
      mos_3m = ifelse(amc_3m == 0 | is.na(amc_3m), NA_real_, stock_on_hand / amc_3m),
      stock_status = case_when(
        stock_on_hand == 0 ~ "Stock out",
        stock_on_hand > 0 & (amc_3m == 0 | is.na(amc_3m)) ~ "Overstock",
        prog_copy != "Vaccines" & mos_3m >= 0 & mos_3m < 2 ~ "Understock",
        prog_copy == "Vaccines" & mos_3m >= 0 & mos_3m < 1 ~ "Understock",
        prog_copy != "Vaccines" & mos_3m >= 2 & mos_3m <= 4 ~ "Adequate",
        prog_copy == "Vaccines" & mos_3m >= 1 & mos_3m <= 2 ~ "Adequate",
        prog_copy != "Vaccines" & mos_3m > 4 ~ "Overstock",
        prog_copy == "Vaccines" & mos_3m > 2 ~ "Overstock",
        TRUE ~ NA_character_
      ),
      satp = case_when(
        stock_status == "Adequate" ~ 1,
        stock_status %in% c("Stock out", "Understock", "Overstock") ~ 0,
        TRUE ~ NA_integer_
      ),
      stockout = case_when(
        stock_status == "Stock out" ~ 1,
        stock_status %in% c("Adequate", "Understock", "Overstock") ~ 0,
        TRUE ~ NA_integer_
      )
    ) %>%
    arrange(serial_no, site_code, desc(date)) %>%
    distinct(site_code, serial_no, date, .keep_all = TRUE)
  
  return(clean_data)
}

# Execute the main processing
tryCatch({
  clean_data <- process_ethiopia_itt_data()
  
  # Create output directory
  output_dir <- "data/ethiopia-itt/clean"
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Identify missing data
  missing_data <- clean_data %>% 
    filter(is.na(site_code)) %>% 
    distinct(facility)
  
  missing_serial <- clean_data %>% 
    filter(is.na(serial_no)) %>% 
    distinct(item)
  
  # Print diagnostics
  cat("Missing site codes for facilities:\n")
  print(missing_data)
  cat("\nMissing serial numbers for items:\n")
  print(missing_serial)
  
  # Run validation tests and save data
  output_csv_path <- file.path(output_dir, "clean_data.csv")
  test_missing(clean_data, output_csv_path)
  
  # Generate reports summary
  reports <- clean_data %>%
    distinct(month, date, site_code, facility) %>%
    arrange(date, site_code)
  
  fwrite(reports, file.path(output_dir, "reports.csv"))
  
  cat("\nProcessing completed successfully!\n")
  cat(glue("Total records processed: {nrow(clean_data)}\n"))
  cat(glue("Unique facilities: {length(unique(clean_data$site_code))}\n"))
  cat(glue("Date range: {min(clean_data$date, na.rm = TRUE)} to {max(clean_data$date, na.rm = TRUE)}\n"))
  
}, error = function(e) {
  cat("Error in processing:", e$message, "\n")
  traceback()
})
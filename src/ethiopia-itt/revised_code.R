# Install and load required libraries
#install.packages(c("googledrive", "readxl", "dplyr"))
library(googledrive)
library(readxl)
library(data.table)
library(tidyverse)
library(tidyverse)
library(purrr)
library(glue)

source("utils.R")
source("tests.R")
## Products
clean_product_list <- fread("products.csv") %>%
  select(serial_no, product, programme) %>%
  rename(sn = serial_no,
         prog = programme)

products_copy <- clean_product_list %>%
  rename(prog_copy = prog,
         item_copy = product)

# Authenticate with Google Drive
drive_auth("daniel_maangi@insupplyhealth.com") # Follow the prompts to authenticate

# Get the parent folder containing subfolders for each month
parent_folder_name <- "https://drive.google.com/drive/folders/1KVm2HCGst-E8XtZJwZ-R70sdzE5dG5rF"
parent_folder <- drive_get(parent_folder_name)

# List all items in the parent folder
subfolders <- drive_ls(parent_folder, type = "folder")

# Check the structure of the `subfolders` data frame
print(subfolders)

# Ensure the 'data' folder exists in the R project
data_dir <- "data"
if (!dir.exists(data_dir)) dir.create(data_dir)

# Initialize a list to store all data
all_data <- list()

# Loop through each subfolder to process Excel files
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
  
  # Read all sheets from the downloaded files
  folder_data <- lapply(files$name, function(file_name) {
    file_path <- file.path(data_dir, file_name)
    sheets <- excel_sheets(file_path)
    
    lapply(sheets, function(sheet) {
      read_excel(file_path, 
                 sheet = sheet, 
                 range = "A3:G45",
                 col_types = "text")
    })
  })
  names(folder_data) <- files$name
  
  # Store data for the current folder
  all_data[[folder_name]] <- folder_data
}

# Optionally combine data from all folders
combined_data <- bind_rows(
  lapply(all_data, function(folder) {
    bind_rows(
      lapply(folder, function(file) {
        bind_rows(file, .id = "sheet_name")
      }),
      .id = "file_name"
    )
  }),
  .id = "folder_name"
)

# Clean
# Renaming columns based on their position
colnames(combined_data)[1] <- "month"
colnames(combined_data)[2] <- "facility"
colnames(combined_data)[4] <- "serial_no"
colnames(combined_data)[5] <- "item"
colnames(combined_data)[6] <- "category"
colnames(combined_data)[7] <- "unit"
colnames(combined_data)[8] <- "stock_on_hand"
colnames(combined_data)[9] <- "consumed"
colnames(combined_data)[10] <- "days_stocked_out"

combined_data <- combined_data[1:10]

clean_data <- combined_data %>%
  mutate(across(c(stock_on_hand, consumed, days_stocked_out), as.numeric)) %>%
  mutate(
    serial_no = as.integer(serial_no),
    stock_on_hand = ifelse(is.na(stock_on_hand), 0, as.numeric(stock_on_hand)),
    consumed = ifelse(is.na(consumed), 0, as.numeric(consumed)),
    days_stocked_out = ifelse(is.na(days_stocked_out), 0, as.numeric(days_stocked_out))
  ) %>%
  mutate(
    adjust_factor = case_when(is.na(days_stocked_out) | days_stocked_out == 0 ~ 1,
                              TRUE ~ 30.5 / days_stocked_out),
    adjusted_consumption = consumed * adjust_factor
  ) %>%
  mutate(
    na_count = rowSums(select(., stock_on_hand, consumed, days_stocked_out), na.rm = TRUE)
  ) %>%
  filter(na_count != 0) %>%
  filter(str_detect(month, "\\b\\d{4}\\b")) %>%
  mutate(
    date = dmy(paste0("1 ", month))
  ) %>%
  # Facility Cleaning
  mutate(
    site_code = case_when(
      str_detect(str_to_lower(facility), "addis") ~ "diredawa-diredawa-addisketemahealthcenter",
      str_detect(str_to_lower(facility), "biyoawale") ~ "diredawa-diredawa-biyoawalehealthcenter",
      str_detect(str_to_lower(facility), "boren") ~ "diredawa-diredawa-boren(industrymender)healthcenter",
      str_detect(str_to_lower(facility), "dechatu") ~ "diredawa-diredawa-dechatuhealthcenter",
      ## Dire Dawa
      str_detect(str_to_lower(facility), "diredawa|dd|dire") ~ "diredawa-diredawa-diredawa(nemberwan)healthcenter",
      
      str_detect(str_to_lower(facility), "gendegerada") ~ "diredawa-diredawa-gendegeradahealthcenter",
      str_detect(str_to_lower(facility), "gendekore") ~ "diredawa-diredawa-gendekorehealthcenter",
      # Goro Health Center
      str_detect(str_to_lower(facility), "goro.xlsx|goro health|goro hc") ~ "diredawa-diredawa-gorohealthcenter",
      str_detect(str_to_lower(facility), "jeldesa|jeldessa") ~ "diredawa-diredawa-jeldesahealthcenter",
      str_detect(str_to_lower(facility), "jelobelina|jelobeline") ~ "diredawa-diredawa-jelobelinahealthcenter",
      ## Kalecha
      str_detect(str_to_lower(facility), "kalecha") ~ "diredawa-diredawa-kalechahealthcenter",
      str_detect(str_to_lower(facility), "kalicha") ~ "diredawa-diredawa-kalechahealthcenter",
      
      #LegeOda
      str_detect(str_to_lower(facility), "legeoda|lege oda|laga") ~ "diredawa-diredawa-legeodagudenfetahealthcenter",
      str_detect(str_to_lower(facility), "lege oda") ~ "diredawa-diredawa-legeodagudenfetahealthcenter",
      
      str_detect(str_to_lower(facility), "legehare") ~ "diredawa-diredawa-legeharehealthcenter",
      str_detect(str_to_lower(facility), "melkajebdu|melka jebdu") ~ "diredawa-diredawa-melkajebduhealthcenter",
      
      # Melkakero HC
      str_detect(str_to_lower(facility), "melkaqero|melkakero|melka kero") ~ "diredawa-diredawa-melkaqerohealthcenter",
      str_detect(str_to_lower(facility), "wahil") ~ "diredawa-diredawa-wahilhealthcenter",
      str_detect(str_to_lower(facility), "dilchora") ~ "diredawa-diredawa-dilchorageneralhospital",
      str_detect(str_to_lower(facility), "sabiyan|sabian") ~ "diredawa-diredawa-sabiyangeneralhospital",
      str_detect(str_to_lower(facility), "ahmadimam|ahemdimam|ahmedimam|ahmed imam") ~ "oromia-jarso-ahmadimamhealthcenter",
      str_detect(str_to_lower(facility), "ale health|alle") ~ "oromia-jarso-alehealthcenter",
      str_detect(str_to_lower(facility), "ananomite|ananoo|anano") ~ "oromia-jarso-ananomitehealthcenter",
      ## same sites
      str_detect(str_to_lower(facility), "ejersa goro") ~ "oromia-jarso-ejersagoro/jarsohealthcenter",
      str_detect(str_to_lower(facility), "jarso health") ~ "oromia-jarso-ejersagoro/jarsohealthcenter",
      ##
      str_detect(str_to_lower(facility), "kora") ~ "oromia-jarso-koramitehealthcenter",
      TRUE ~ NA_character_)
  ) %>%
  left_join(clean_product_list, by = c("item" = "product")) %>%
  mutate(
    serial_no = case_when(!is.na(serial_no) & is.na(sn) ~ serial_no,
                         is.na(serial_no) & !is.na(sn) ~ sn,
                         TRUE ~ serial_no
                         )
  )

## Clean additional serials
clean_data <- clean_data %>%
  mutate(
    serial_no = case_when(
      str_detect(item, "Albendazole - 400mg – Tablet") ~ 5,
      str_detect(item, "Amoxicillin – 250mg") ~ 6,
      str_detect(item, "Ciprofloxacin") ~ 10,
      str_detect(item, "TTC eye ointment") ~ 19,
      str_detect(item, "Arthmeter") ~ 25,
      str_detect(item, "Amlodipine") ~ 26,
      str_detect(item, "Frusamide") ~ 27,
      str_detect(item, "Hydralizine") ~ 28,
      str_detect(item, "Metformin") ~ 29,
      str_detect(item, "Adrenaline") ~ 6,
      str_detect(item, "Metronidazole") ~ 31,
      str_detect(item, "Normal Saline") ~ 33,
      str_detect(item, "Omeprazole") ~ 34,
      str_detect(item, "Tetanus Anti") ~ 35,
      str_detect(item, "TDF/3TC/DTG") ~ 17,
      str_detect(item, "RHZE") ~ 24,
      str_detect(item, "TDF/3TC/DTG") ~ 17,
      TRUE ~ serial_no)
  ) %>%
  select(-sn) %>%
  filter(item != "190") %>% # Product does not exist
arrange(site_code, serial_no, date) %>%
  as.data.table() %>%
  left_join(products_copy, by = c("serial_no" = "sn"))


# clean_data[, consumption := ifelse(is.na(consumption), 0, consumption)]
clean_data[, consumed_in_6m := frollsum(consumed, n = 6, na.rm = TRUE), 
             by = .(site_code, serial_no)]

# calculate amc @ site
library(zoo)
clean_data[, amc_3m := zoo::rollapply(adjusted_consumption, width = 3, FUN = function(x) mean(x, na.rm = TRUE), 
                                      partial = TRUE, align = "right"), 
           by = .(site_code, serial_no)]



# SATP Calculations
clean_data <- clean_data |>
  mutate(
    mos_3m = ifelse(amc_3m == 0, NA_real_ , stock_on_hand / amc_3m)
  ) %>%
  mutate(
    stock_status = case_when(stock_on_hand == 0 ~ "Stock out",
                             stock_on_hand > 0 & (amc_3m == 0 | is.na(amc_3m)) ~ "Overstock",
                             prog_copy != "Vaccines" & mos_3m >= 0 & mos_3m < 2 ~ "Understock",
                             prog_copy == "Vaccines" & mos_3m >= 0 & mos_3m < 1 ~ "Understock",
                             prog_copy != "Vaccines" & mos_3m >= 2 & mos_3m <= 4 ~ "Adequate",
                             prog_copy == "Vaccines" & mos_3m >= 1 & mos_3m <= 2 ~ "Adequate",
                             prog_copy != "Vaccines" & mos_3m > 4 ~ "Overstock",
                             prog_copy == "Vaccines" & mos_3m > 2 ~ "Overstock",
                             TRUE ~ NA_character_)
  ) %>%
  mutate(
    satp = case_when(stock_status == "Adequate" ~ 1,
                     stock_status %in% c("Stock out", "Understock", "Overstock") ~ 0,
                     TRUE ~ NA_integer_
                     ),
    stockout = case_when(stock_status == "Stock out" ~ 1,
                     stock_status %in% c("Adequate", "Understock", "Overstock") ~ 0,
                     TRUE ~ NA_integer_
    )
  ) %>%
  arrange(serial_no, site_code, desc(date)) %>%
  distinct(site_code, serial_no, date, .keep_all = TRUE)
  

# Missing data
missing_data <- clean_data %>% 
  filter(is.na(site_code)) |> 
  distinct(facility)
#-----------------------------
missing_serial <- clean_data %>% 
  filter(is.na(serial_no)) %>% 
  distinct(item)

# Run tests
output_csv_path <- "clean_data.csv"
test_missing(clean_data, output_csv_path)

# Generate reports
reports <- clean_data %>%
  distinct(month, date, site_code)
fwrite(reports, "reports.csv")















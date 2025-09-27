# Load required libraries
library(fastLink)
library(readxl)
library(dplyr)
library(stringr)

# Read the Excel file (first worksheet)
excel_data <- read_excel("C:\\Users\\uat6\\Downloads\\RRFs\\Use these RRFs\\April RRF\\Ejere HC April.xlsx", 
                         sheet = 1) 

# Read the CSV file
csv_data <- read.csv("C:\\Users\\uat6\\Downloads\\RRFs\\dhis2_metadata_categoryoptioncombos.csv", 
                     stringsAsFactors = FALSE) %>%
  mutate(excel_row_id = row_number())

# Extract product names from Excel file
excel_products <- excel_data %>%
  # Select the product description column and issue unit column
  select(product_name = 2, issue_unit = 7) %>%
  mutate(excel_row_id = row_number()) %>%
  filter(product_name != "Product Description") %>%
  filter(!is.na(issue_unit)) %>%
  # Remove rows where product_name is NA or empty
  filter(!is.na(product_name), 
         product_name != "", 
         # Remove category headers (those containing "A.", "B.", "C." etc.)
         !str_detect(product_name, "^[A-Z][0-9]*\\."),
         # Remove rows that are clearly headers or categories
         !str_detect(product_name, "(ARVs|Line|Part)$")) %>%
  # Clean the product names for better matching
  mutate(product_name_clean = str_trim(product_name),
         # Convert issue_unit to character for consistent matching
         unit = as.character(issue_unit)) %>%
  # Remove duplicates
  distinct(product_name_clean, .keep_all = TRUE) %>%
  # Select final columns with consistent names
  select(excel_row_id, product_name = product_name_clean, unit, original_excel_name = product_name)

# Prepare CSV data with same column structure
csv_products <- csv_data %>%
  select(name, id) %>%
  filter(!is.na(name), name != "") %>%
  mutate(csv_row_id = row_number(),
         # Extract the core product name from the structured format
         product_name = str_extract(name, "(?<=: )[^,]+"),
         # Extract units using the last comma - get everything after last comma
         unit = str_extract(name, "[^,]+$"),
         # Trim whitespace from both
         product_name = str_trim(product_name),
         unit = str_trim(unit)) %>%
  # Select final columns with same names as excel_products
  select(csv_row_id, product_name, unit, name, id)

# Display data structure for verification
cat("Excel products structure:\n")
cat("Columns:", names(excel_products), "\n")
cat("First few rows:\n")
print(head(excel_products, 3))

cat("\nCSV products structure:\n")
cat("Columns:", names(csv_products), "\n")
cat("First few rows:\n")
print(head(csv_products, 3))

# Perform probabilistic matching using fastLink
matches <- fastLink(
  dfA = excel_products,
  dfB = csv_products,
  varnames = c("product_name", "unit"),
  stringdist.match = c("product_name", "unit"),
  partial.match = c("product_name"),
  # Adjust threshold for sensitivity (lower = more matches, higher = more precise)
  threshold.match = 0.9,
  # Use Jaro-Winkler for string matching (good for names)
  stringdist.method = "jw",
  # Allow for some variation in string lengths
  cut.a = 0.92,
  cut.p = 0.88
)

# Extract matched pairs
matched_pairs <- getMatches(
  dfA = excel_products,
  dfB = csv_products,
  fl.out = matches
)


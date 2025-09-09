library(testthat)

# Test output before downstream processsing
test_missing <- function(data, output_csv_path) {
  test_that("Check for missing values", {
    # Check if site_code column exists
    expect_true("site_code" %in% colnames(data), "The 'site_code' column is missing from the dataset.")
    expect_true("serial_no" %in% colnames(data), "The 'serial_no' column is missing from the dataset.")
    
    # Check for missing values in site_code
    site_missing_count <- sum(is.na(data$site_code))
    item_code <- sum(is.na(data$serial_no))
    
    expect_equal(site_missing_count, 0, info = glue("There are {site_missing_count} missing values in 'site_code'."))
    expect_equal(item_code, 0, info = glue("There are {item_code} missing values in 'serial_no'."))
    
    # Handle data based on missing_count
    if (site_missing_count == 0 | item_code == 0) {
      fwrite(data, output_csv_path)
      cat(glue("Data saved to {output_csv_path}\n"))
    } 
  })
}

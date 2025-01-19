library(testthat)
library(dplyr)
library(data.table)
library(glue)
library(blastula)

# Define the test function
test_no_missing_site_code <- function(data, output_csv_path, email) {
  test_that("Check for missing values in site_code", {
    # Ensure the site_code column exists
    expect_true("site_code" %in% colnames(data), "The 'site_code' column is missing from the dataset.")
    
    # Check for missing values in site_code
    missing_count <- sum(is.na(data$site_code))
    expect_equal(missing_count, 0, info = glue("There are {missing_count} missing values in 'site_code'."))
    
    if (missing_count == 0) {
      # Save the data as a CSV if there are no missing values
      fwrite(data, output_csv_path)
      cat(glue("Data saved to {output_csv_path}\n"))
    } else {
      # Send an email if there are missing values
      send_email(email, missing_count)
    }
  })
}

# Function to simulate sending an email
send_email <- function(email, missing_count) {
  # Create the email body
  email_body <- glue::glue("There are {missing_count} missing values in 'site_code'. Please check and clean the data.")
  
  # Create the email message
  email_message <- compose_email(
    body = md(email_body)
  )
  
  # Send the email
  smtp_send(
    email = email_message,
    from = "dmaangi@statbricks.com",  # Replace with your email
    to = email,
    subject = "Ethiopia ITT Dashboard: Missing Values Alert in site_code",
    credentials = creds_key(
      id = "your_smtp_credentials"  # Setup SMTP credentials beforehand
    )
  )
  
  cat(glue("Email sent to {email} notifying {missing_count} missing values in 'site_code'.\\n"))
}


# Usage
# Replace `clean_data` with the actual dataset
output_csv_path <- "clean_data.csv"
email <- "dmaangi@statbricks.com"
test_no_missing_site_code(clean_data, output_csv_path, email)
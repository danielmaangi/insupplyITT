
months <- paste0(period, sprintf("%02d", 1:12), collapse = ";")

login(username,password,base.url)

# Construct the API URL
reports_url <- paste0(base.url,
                     "api/40/analytics.csv?dimension=dx:ppdqEN91qZs.EXPECTED_REPORTS;",
                     "ppdqEN91qZs.ACTUAL_REPORTS;ppdqEN91qZs.ACTUAL_REPORTS_ON_TIME&",
                     "dimension=pe:", months, "&",
                     "dimension=ou:HfVjCurKxh2;LEVEL-5")

# Fetch the data
response <- GET(reports_url)
hpt_reports <- read_csv(content(response, as = "text"))
write_csv(hpt_reports,
          paste0("G:/.shortcut-targets-by-id/1-I-AM6fIfll60idw-jnzME08IVKzNdfj/MLE_DATA/Dashboards/Production/HPT/dataModel/DHIS2/hiskenya/Download/REPORTS/HPT_REPORTS_", period, ".csv"))


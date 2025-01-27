#-----------------------------------------------------------------------
source(paste0(project_path, "R/functions.R"))
login(username, password, base.url)
source(paste0(project_path, "setup.R"))
#--------------------------------------------------------------------

# ----------------------------------------------------------------------------------------------------
pop_des <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:in:[XuVxScOosIt]", 
                                  fields = "name, id, shortName, description, formName,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  select(name, id, shortName, formName,domainType, valueType, categoryCombo.id) 

# Define the URL with parameters
url <- paste0("https://hiskenya.org/api/dataValueSets?",
              "dataSet=XuVxScOosIt",
              "&startDate=2024-01-01&endDate=2024-12-31",
              "&orgUnit=HfVjCurKxh2&children=true")

# Make the GET request and parse the JSON response
response <- GET(url, timeout(120))  # Timeout of 2 minutes
content <- content(response, "text")
data <- fromJSON(content)

# Convert the data to a data frame
data_df <- as.data.frame(data$dataValues)

# Select and rename the relevant columns
data_clean <- data_df %>%
  left_join(pop_des, by = c("dataElement" = "id")) |>
  select(
    dataElement,
    period,
    orgUnit,
    categoryOptionCombo,
    attributeOptionCombo,
    name,
    value
  ) |>
  mutate(value = as.numeric(value)) # |>
  # pivot_wider(names_from = name,
  #             values_from = value)

dbWriteTable(con, "estimates", data_clean, overwrite = TRUE, row.names = FALSE)
#rm(list = ls())


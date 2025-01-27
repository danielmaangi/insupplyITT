rm(list = ls())
project_path <- "~/projects/insupplyHealth/kenya_itt/data/kenya_itt/"
source(paste0(project_path, "setup.R"))

# Read all CSV files into a single data frame using map_df
fp_data_reports <- map_df(list.files(paste0(project_path, "raw/Reports/FP/"), 
                              pattern = "\\.csv$", full.names = TRUE),
                          fread) |>
  mutate(Programme = "Family Planning")

imm_data_reports <- map_df(list.files(paste0(project_path, "raw/Reports/Immunization/"), 
                                     pattern = "\\.csv$", full.names = TRUE),
                          fread) |>
  mutate(Programme = "Immunization")

mal_data_reports <- map_df(list.files(paste0(project_path, "raw/Reports/Malaria/"), 
                                     pattern = "\\.csv$", full.names = TRUE),
                          fread) |>
  mutate(Programme = "Malaria")

nut_data_reports <- map_df(list.files(paste0(project_path, "raw/Reports/Nutrition/"), 
                                      pattern = "\\.csv$", full.names = TRUE),
                           fread) |>
  mutate(Programme = "Nutrition")

all_reports <- bind_rows(fp_data_reports, 
                         imm_data_reports, 
                         mal_data_reports, 
                         nut_data_reports) |>
  separate_wider_delim(Data, ".", names = c("Dataset ID", "Indicator")) |>
  pivot_wider(names_from = Indicator,
              values_from = Value) |>
  filter(EXPECTED_REPORTS > 0)

fwrite(all_reports, paste0(project_path, "transform/reports.csv"))

programs <- all_reports |>
  distinct(`Dataset ID`, Programme)

# Reports
dbWriteTable(con, "reports", all_reports, overwrite = TRUE, row.names = FALSE)





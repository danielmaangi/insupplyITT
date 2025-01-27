#-----------------------------------------------------------------------
source("src/kenya-itt/functions.R")
login(username, password, base.url)
source("src/kenya-itt/setup.R")
#--------------------------------------------------------------------

# Download FP Data
login(username, password, base.url)
monthly_download(fp_dataset, 
                 startdate, enddate, 
                 orgunit, 
                 "data/kenya-itt/raw/results/Family Planning/")

login(username, password, base.url)
monthly_download(imm_dataset, 
                 startdate, enddate, 
                 orgunit, 
                 "data/kenya-itt/raw/results/Immunization/")

login(username, password, base.url)
monthly_download(mal_dataset, 
                 startdate, enddate, 
                 orgunit, 
                 "data/kenya-itt/raw/results/Malaria/")

login(username, password, base.url)
monthly_download(nut_dataset, 
                 startdate, enddate, 
                 orgunit, 
                 "data/kenya-itt/raw/results/Nutrition/")

# Population data
login(username, password, base.url)
monthly_download(pop_dataset, 
                 startdate, enddate, 
                 orgunit, 
                 "data/kenya-itt/raw/results/Population/")


# Reporting Rates
login(username, password, base.url)
fetch_reports(fp_dataset, 
              startdate, enddate, 
              orgunit, 
              "data/kenya-itt/raw/reports/FP/")

login(username, password, base.url)
fetch_reports(imm_dataset, 
              startdate, enddate, 
              orgunit, 
              "data/kenya-itt/raw/reports/Immunization/")

login(username, password, base.url)
fetch_reports(mal_dataset, 
              startdate, enddate, 
              orgunit, 
              "data/kenya-itt/raw/reports/Malaria/")

login(username, password, base.url)
fetch_reports(nut_dataset, 
              startdate, enddate, 
              orgunit, 
              "data/kenya-itt/raw/reports/Nutrition/")


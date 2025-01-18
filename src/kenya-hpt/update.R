# SETUP
#Required Libraries
library(httr)
library(httr2)
library(rjson)
library(tidyverse)
library(data.table)
library(readxl)
library(jsonlite)
library(future.apply)


#Kenya ~ HfVjCurKxh2
username<-"Maangi"
password<-"kiMaNi:1991"
base.url<-"https://hiskenya.org/"
period <- 2024
startdate <- paste0(period, "-01-01")
enddate <- paste0(period, "-12-31")
country <- "Kenya" # Type country
action <- "download"

orgunit <- case_when(
  country == "Kenya" ~ "HfVjCurKxh2",
  TRUE ~ ""
)


login <- function(username,password,base.url) {
  url<-paste0(base.url,"api/me")
  r<-GET(url,authenticate(username,password))
  
  if(r$status == 200L) { print("Logged in successfully!")} 
  else {print("Could not login")}
}
login(username,password,base.url)

#source("G:/.shortcut-targets-by-id/1-I-AM6fIfll60idw-jnzME08IVKzNdfj/MLE_DATA/Dashboards/Production/HPT/dataModel/DHIS2/hiskenya/codes/Download.R")
#source("G:/.shortcut-targets-by-id/1-I-AM6fIfll60idw-jnzME08IVKzNdfj/MLE_DATA/Dashboards/Production/HPT/dataModel/DHIS2/hiskenya/codes/reports.R")

###########
source("G:/.shortcut-targets-by-id/1-I-AM6fIfll60idw-jnzME08IVKzNdfj/MLE_DATA/Dashboards/Production/HPT/dataModel/DHIS2/hiskenya/codes/parameters.R")
# source("G:/.shortcut-targets-by-id/1-I-AM6fIfll60idw-jnzME08IVKzNdfj/MLE_DATA/Dashboards/Production/HPT/dataModel/DHIS2/hiskenya/codes/reports.R")
# source("G:/.shortcut-targets-by-id/1-I-AM6fIfll60idw-jnzME08IVKzNdfj/MLE_DATA/Dashboards/Production/HPT/dataModel/DHIS2/hiskenya/codes/RevisedDownload.R")


############
source("G:/.shortcut-targets-by-id/1-I-AM6fIfll60idw-jnzME08IVKzNdfj/MLE_DATA/Dashboards/Production/HPT/dataModel/DHIS2/hiskenya/codes/utils.R")
source("G:/.shortcut-targets-by-id/1-I-AM6fIfll60idw-jnzME08IVKzNdfj/MLE_DATA/Dashboards/Production/HPT/dataModel/DHIS2/hiskenya/codes/Merge.R")
source("G:/.shortcut-targets-by-id/1-I-AM6fIfll60idw-jnzME08IVKzNdfj/MLE_DATA/Dashboards/Production/HPT/dataModel/DHIS2/hiskenya/codes/lastUpdate.R")

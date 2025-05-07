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
# devtools::install_github(repo = "https://github.com/pepfar-datim/datimutils.git", ref = "master")
library(datimutils)


Kenya ~ HfVjCurKxh2
#username<-"Maangi"
#password<-"kiMaNi:1991"
username<-"mikonya"
password<-"Kenya2030"
base.url<-"https://hiskenya.org/"
period <- 2025
enddate <- Sys.Date() - months(1)  #Sys.Date(); as.Date("2023-01-01") / 
startdate <- enddate - months(6)  
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


############
source("src/kenya-hpt/metadata.R")
source("src/kenya-hpt/orgunits.R")
source("src/kenya-hpt/utils.R")
source("src/kenya-hpt/Merge.R")
source("src/kenya-hpt/lastUpdate.R")
source("src/kenya-hpt/deploy.R")


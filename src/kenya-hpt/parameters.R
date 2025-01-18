# SETUP
#Required Libraries
library(httr)
library(httr2)
library(rjson)
library(tidyverse)
library(data.table)
library(readxl)
library(jsonlite)


#Kenya ~ HfVjCurKxh2
username<-"Maangi"
password<-"kiMaNi:1991"
base.url<-"https://hiskenya.org/"
period <- 2024
startdate_01 <- paste0(period, "-01-01")
enddate_01 <- paste0(period, "-03-31")

startdate_02 <- paste0(period, "-04-01")
enddate_02 <- paste0(period, "-06-30")

startdate_03 <- paste0(period, "-07-01")
enddate_03 <- paste0(period, "-09-30")

startdate_04 <- paste0(period, "-10-01")
enddate_04 <- paste0(period, "-12-31")




country <- "Kenya" # Type country
action <- "download"

orgunit <- case_when(
  country == "Kenya" ~ "HfVjCurKxh2",
  TRUE ~ ""
)


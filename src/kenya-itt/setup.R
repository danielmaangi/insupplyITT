source("src/kenya-itt/packages.R")
source("config/kenya-itt/credentials.R")
source("src/kenya-itt/functions.R")
# dhis2 session
loginToDATIM(username = username,
             password = password,
             base_url = base.url)

# Specify query parameters
orgunit <- 'HfVjCurKxh2'
enddate <- Sys.Date() - months(1)  #Sys.Date(); as.Date("2023-01-01") / 
startdate <- enddate - months(11)  



#----------------------------------------------------------------------------------------------
# 	MOH 647 Facility Tracer Health Products and Technology (HPT) Data Report Form: ppdqEN91qZs
#---------------------------------------------------------------------------------------------
# Q1

login <- function(username,password,base.url) {
  url<-paste0(base.url,"api/me")
  r<-GET(url,authenticate(username,password))
  
  if(r$status == 200L) { print("Logged in successfully!")} 
  else {print("Could not login")}
}
login(username,password,base.url)

hpt_url_01 <- paste0(base.url,
                  "api/dataValueSets?dataSet=ppdqEN91qZs&&startDate=", 
                  startdate_01, "&endDate=", enddate_01,"&orgUnit=",orgunit, "&children=true")
#get_json <- GET(hpt_url)

hpt_01 <- content(GET(hpt_url_01), as = "parsed")

hpt_q1 <- rbindlist(hpt_01$dataValues, fill = TRUE)[,1:6]
dim(hpt_q1)

# Q2

login <- function(username,password,base.url) {
  url<-paste0(base.url,"api/me")
  r<-GET(url,authenticate(username,password))
  
  if(r$status == 200L) { print("Logged in successfully!")} 
  else {print("Could not login")}
}
login(username,password,base.url)

hpt_url_02 <- paste0(base.url,
                     "api/dataValueSets?dataSet=ppdqEN91qZs&&startDate=", 
                     startdate_02, "&endDate=", enddate_02,"&orgUnit=",orgunit, "&children=true")
#get_json <- GET(hpt_url)

hpt_02 <- content(GET(hpt_url_02), as = "parsed")

hpt_q2 <- rbindlist(hpt_02$dataValues, fill = TRUE)[,1:6]
dim(hpt_q2)


# Q3

login <- function(username,password,base.url) {
  url<-paste0(base.url,"api/me")
  r<-GET(url,authenticate(username,password))
  
  if(r$status == 200L) { print("Logged in successfully!")} 
  else {print("Could not login")}
}
login(username,password,base.url)

hpt_url_03 <- paste0(base.url,
                     "api/dataValueSets?dataSet=ppdqEN91qZs&&startDate=", 
                     startdate_03, "&endDate=", enddate_03,"&orgUnit=",orgunit, "&children=true")
#get_json <- GET(hpt_url)

hpt_03 <- content(GET(hpt_url_03), as = "parsed")

hpt_q3 <- rbindlist(hpt_03$dataValues, fill = TRUE)[,1:6]
dim(hpt_q3)


# Q4

login <- function(username,password,base.url) {
  url<-paste0(base.url,"api/me")
  r<-GET(url,authenticate(username,password))
  
  if(r$status == 200L) { print("Logged in successfully!")} 
  else {print("Could not login")}
}
login(username,password,base.url)

hpt_url_04 <- paste0(base.url,
                     "api/dataValueSets?dataSet=ppdqEN91qZs&&startDate=", 
                     startdate_04, "&endDate=", enddate_04,"&orgUnit=",orgunit, "&children=true")
#get_json <- GET(hpt_url)

hpt_04 <- content(GET(hpt_url_04), as = "parsed")

hpt_q4 <- rbindlist(hpt_04$dataValues, fill = TRUE)[,1:6]
dim(hpt_q4)



hpt <- bind_rows(hpt_q1, hpt_q2, hpt_q3, hpt_q4)

# View(HPT)

write_csv(hpt,
          paste0("G:/.shortcut-targets-by-id/1-I-AM6fIfll60idw-jnzME08IVKzNdfj/MLE_DATA/Dashboards/Production/HPT/dataModel/DHIS2/hiskenya/Download/HPT/HPT_", period, ".csv"))













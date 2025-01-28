#----------------------------------------------------------------------------------------------
# 	MOH 647 Facility Tracer Health Products and Technology (HPT) Data Report Form: ppdqEN91qZs
#---------------------------------------------------------------------------------------------
hpt_url <- paste0(base.url,
                       "api/dataValueSets?dataSet=ppdqEN91qZs&&startDate=", startdate, "&endDate=", enddate,"&orgUnit=",orgunit, "&children=true")

hpt_01 <- content(GET(hpt_url), as = "parsed")

hpt <- rbindlist(hpt_01$dataValues, fill = TRUE)

hpt <- hpt[,1:6]

# View(HPT)

write_csv(hpt,
          paste0("G:/.shortcut-targets-by-id/1-I-AM6fIfll60idw-jnzME08IVKzNdfj/MLE_DATA/Dashboards/Production/HPT/dataModel/DHIS2/hiskenya/Download/HPT/HPT_", period, ".csv"))














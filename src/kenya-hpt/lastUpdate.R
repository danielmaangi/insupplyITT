# Dataset paths

hpt_path <- "G:/.shortcut-targets-by-id/1-I-AM6fIfll60idw-jnzME08IVKzNdfj/MLE_DATA/Dashboards/Production/HPT/dataModel/DHIS2/hiskenya/Download/HPT.csv"

# Last updated

dataSet = c("HPT")
lastUpdate = c(
 as.character(tail(file.info(hpt_path)$mtime))
)


latestUpdate <- cbind(
  dataSet,
  lastUpdate
) %>%
  as.data.frame()

fwrite(latestUpdate,
       paste0("G:/.shortcut-targets-by-id/1-I-AM6fIfll60idw-jnzME08IVKzNdfj/MLE_DATA/Dashboards/Production/HPT/dataModel/DHIS2/hiskenya/Download/latestUpdate", ".csv"))

# Dataset paths

hpt_path <- "data/kenya-hpt/clean/HPT.csv"

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
       paste0("data/kenya-hpt/clean/latestUpdate", ".csv"))

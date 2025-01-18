library(taskscheduleR)
myscript <- "G:/My Drive/Personal/MLE_DATA/DataUse/dataModel/DHIS2/hiskenya/codes/update.R"

## Run every day at the same time on 09:10, starting from tomorrow on
## Mark: change the format of startdate to your locale if needed (e.g. US: %m/%d/%Y)
taskscheduler_create(taskname = "ittdaily", 
                     rscript = myscript, 
                     schedule = "DAILY", 
                     starttime = "19:12", startdate = format(Sys.Date(), "%d/%m/%y"))

mergescript <- "G:/My Drive/Personal/MLE_DATA/DataUse/dataModel/DHIS2/hiskenya/codes/Merge.R"

taskscheduler_create(taskname = "ittMerge", 
                     rscript = mergescript, 
                     schedule = "DAILY", 
                     starttime = "07:08", startdate = format(Sys.Date(), "%d/%m/%y"))




library(taskscheduleR)
Jobplanet <- file.path(getwd(),"Merge.R")
taskscheduler_create(taskname = "ittMinute", rscript = Jobplanet, 
                     schedule = "MINUTE", 
                     starttime = "19:15", 
                     startdate = format(Sys.Date(), "%Y/%m/%d"),
                     days = 31)




## get a data.frame of all tasks
tasks <- taskscheduler_ls()
str(tasks)

## delete the tasks
taskscheduler_delete(taskname = "ittdaily")

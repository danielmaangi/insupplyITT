library(cronR)
# Path to the R script
f <- "~/projects/insupplyHealth/kenya_itt/data/kenya_itt/refresh.R"

# Verify that the file path is correct and accessible
file.exists(f)  # Check if the file exists

# Define the command to execute the R script
cmd <- cron_rscript(f)

# Verify the command is correctly defined
print(cmd)  # Check the command

# Schedule the cron job
cron_add(command = cmd, 
         frequency = 'daily', 
         at = "15:56PM", 
         id = 'test2456', 
         description = 'My process 2')

# cron_add(command = cmd, 
#          frequency = 'hourly', 
#          id = 'test1', 
#          description = 'My process 1')

cron_njobs()
#cronR::cron_clear()

# Test on terminal
# Rscript ~/projects/insupplyHealth/kenya_itt/data/kenya_itt/refresh.R

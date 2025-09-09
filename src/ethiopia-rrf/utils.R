extract_files <- function(subfolders, data_dir) {
  for (i in seq_len(nrow(subfolders))) {
    folder_name <- subfolders$name[i]
    folder_id <- subfolders$id[i]
    
    # List files in the current subfolder
    files <- drive_ls(as_id(folder_id)) %>%
      filter(grepl("\\.xlsx$", name) | grepl("\\.xls$", name))
    
    # Download Excel files locally
    lapply(seq_len(nrow(files)), function(j) {
      drive_download(as_id(files$id[j]), 
                     path = file.path(data_dir, files$name[j]),
                     overwrite = TRUE)
    })
  }
}



read_files <- function(data_dir, subfolders) {
  all_data <- list()
  
  for (i in seq_len(nrow(subfolders))) {
    folder_name <- subfolders$name[i]
    
    # Get file names in the folder
    files <- list.files(path = data_dir, pattern = "\\.xlsx$|\\.xls$", full.names = TRUE)
    
    # Read all sheets from the downloaded files
    folder_data <- lapply(files, function(file_path) {
      sheets <- excel_sheets(file_path)
      
      lapply(sheets, function(sheet) {
        read_excel(file_path, 
                   sheet = sheet, 
                   range = "A3:G45",
                   col_types = "text")
      })
    })
    names(folder_data) <- basename(files)
    
    # Store data for the current folder
    all_data[[folder_name]] <- folder_data
  }
  
  return(all_data)
}


# Define the directory to store data
# data_dir <- "data"

# First, extract the files
# extract_files(subfolders, data_dir)

# Then, read the downloaded files
# all_data <- read_files(data_dir, subfolders)













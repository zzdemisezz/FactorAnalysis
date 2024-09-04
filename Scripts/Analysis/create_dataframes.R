rm(list = ls())
source("Scripts/Analysis/functions.R")
source("Scripts/Analysis/loading-objects.R")

# Specifying the directory results
main_dir <- "Results/results_pxl" 
subdir_paths <- generate_subdir_paths(main_dir)

# # Initialize an empty list to store all results
# all_analysis_results <- list()
# 
# # Loop through each subdir_path and perform the analysis
# for (subdir_path in subdir_paths) {
#   print(paste("Performing analysis for:", subdir_path))
#   
#   # Load data directly
#   analysis_results <- load_data(subdir_path)
#   
#   # Extract the dataset name 
#   dataset_name <- basename(subdir_path)
#   
#   # Store the result in the list with the dataset name as the key
#   all_analysis_results[[dataset_name]] <- analysis_results
# }

all_analysis_results <- readRDS("all_analysis_results.rds")
stop()





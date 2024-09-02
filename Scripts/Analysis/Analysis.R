rm(list = ls())
source("Scripts/Analysis/loading-objects.R")
source("Scripts/Analysis/heatmap.R")

# Set subdir_path
# subdir_path <- "results4/Overlap2_Large_Moderate"
# subdir_path <- "results4/Overlap3_Large_Moderate"
subdir_path <- "results4/LargeFactors_5x5_Strong"
# subdir_path <- "results4/Overlap3_Large_Weak"

# Load data
data_name <- gsub("results4/", "", subdir_path)  # Extract the dataset name from subdir_path
data_name <- gsub("/", "_", data_name)  # Replace slashes with underscores to create a valid variable name

# Load the data and assign to a dynamic variable name
assign(data_name, load_data(subdir_path))
dataframe <- get(data_name)$dataframe
raw_data <- get(data_name)$raw_data

# Statistics matrices 
# EM data
statistics_EM <- calculate_statistics(dataframe, type = "em")

# EM BETA data
statistics_EM_BETA <- calculate_statistics(dataframe, type = "em_beta")

# Calculate metrics for B and B_beta
metrics_B <- calculate_metrics(statistics_EM$bias_matrix, statistics_EM$variance_matrix, statistics_EM$MSE_matrix)
metrics_B_beta <- calculate_metrics(statistics_EM_BETA$bias_matrix, statistics_EM_BETA$variance_matrix, statistics_EM_BETA$MSE_matrix)

# Calculate metrics for Covariance and Covariance_beta
metrics_Covariance <- calculate_metrics(statistics_EM$bias_covariance, statistics_EM$variance_covariance, statistics_EM$MSE_covariance)
metrics_Covariance_beta <- calculate_metrics(statistics_EM_BETA$bias_covariance, statistics_EM_BETA$variance_covariance, statistics_EM_BETA$MSE_covariance)

# Number of times model gets structure right
structure_check_em <- compare_GAMMA_to_B_true(dataframe, type = "em")
structure_check_em_beta <- compare_GAMMA_to_B_true(dataframe, type = "em_beta")
exact_matches_em <- structure_check_em$exact_matches
exact_matches_em_beta <- structure_check_em_beta$exact_matches

# Combine results into data frames with the new information
combined_results <- data.frame(
  Metric = c("Bias", "Variance", "MSE", "Number of Exact Matches"),
  B = c(as.numeric(metrics_B), exact_matches_em),
  B_beta = c(as.numeric(metrics_B_beta), exact_matches_em_beta),
  Covariance = c(as.numeric(metrics_Covariance), NA),  # No equivalent for Covariance
  Covariance_beta = c(as.numeric(metrics_Covariance_beta), NA)  # No equivalent for Covariance
)

# Print the combined results
print(combined_results)

# non_matching_indices_em <- structure_check_em$non_matching_indices  # If you need to use the non-matching indices
# non_matching_indices_em_beta <- structure_check_em_beta$non_matching_indices  # If you need to use the non-matching indices

stop()

# Heatmaps

# Access true matrices (assuming these are consistent across the simulations)
B_true <- dataframe$B_True[[1]]
Covariance_true <- dataframe$Covariance_matrix_true[[1]]

# Generate heatmaps for the EM model
generate_heatmaps(B_true, statistics_EM, paste(data_name, "B Matrix"))
generate_heatmaps(Covariance_true, statistics_EM, paste(data_name, "Covariance Matrix"))

# Generate heatmaps for the EM BETA model
generate_heatmaps(B_true, statistics_EM_BETA, paste(data_name, "B Matrix (Beta)"))
generate_heatmaps(Covariance_true, statistics_EM_BETA, paste(data_name, "Covariance Matrix (Beta)"))




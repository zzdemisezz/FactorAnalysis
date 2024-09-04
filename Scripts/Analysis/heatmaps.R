rm(list = ls())
source("Scripts/Analysis/functions.R")
source("Scripts/Analysis/loading-objects.R")

all_analysis_results <- readRDS("all_analysis_results.rds")

# Load the relevant dataframe
heatmap_dataframe <- all_analysis_results$`overlap2-large-weak`

# Calculate statistics for B_permuted and B_permuted_beta using the calculate_statistics function
statistics_em <- calculate_statistics(heatmap_dataframe, type = "em")
statistics_beta <- calculate_statistics(heatmap_dataframe, type = "em_beta")



# Create heatmaps for B_permuted (from statistics_em)
heatmap_true_loadings <- create_heatmap(statistics_em$B_true, title = "Heatmap of True Loading Matrix")
heatmap_average_loadings <- create_heatmap(statistics_em$average_matrix, title = "Heatmap of Average Loading Matrix")
heatmap_bias_loadings <- create_heatmap(statistics_em$bias_matrix, title = "Heatmap of Bias Loading Matrix")
heatmap_variance_loadings <- create_heatmap(statistics_em$variance_matrix, title = "Heatmap of Variance Loading Matrix")
heatmap_mse_loadings <- create_heatmap(statistics_em$MSE_matrix, title = "Heatmap of MSE Loading Matrix")

# Create heatmaps for B_permuted_beta (from statistics_beta)
heatmap_average_loadings_beta <- create_heatmap(statistics_beta$average_matrix, title = "Heatmap of Average Loading Matrix - Beta")
heatmap_bias_loadings_beta <- create_heatmap(statistics_beta$bias_matrix, title = "Heatmap of Bias Loading Matrix - Beta")
heatmap_variance_loadings_beta <- create_heatmap(statistics_beta$variance_matrix, title = "Heatmap of Variance Loading Matrix - Beta")
heatmap_mse_loadings_beta <- create_heatmap(statistics_beta$MSE_matrix, title = "Heatmap of MSE Loading Matrix - Beta")

# Print the heatmaps (for example)
print(heatmap_true_loadings)

stop()
print(heatmap_average_loadings)
print(heatmap_bias_loadings)
print(heatmap_variance_loadings)
print(heatmap_mse_loadings)

print(heatmap_average_loadings_beta)
print(heatmap_bias_loadings_beta)
print(heatmap_variance_loadings_beta)
print(heatmap_mse_loadings_beta)

# Covariances ####

# Create heatmaps for Covariance_matrix (from statistics_em)
heatmap_true_covariance <- create_heatmap(statistics_em$Covariance_true, title = "Heatmap of True Covariance Matrix")
heatmap_average_covariance <- create_heatmap(statistics_em$average_covariance, title = "Heatmap of Average Covariance Matrix")
heatmap_bias_covariance <- create_heatmap(statistics_em$bias_covariance, title = "Heatmap of Bias Covariance Matrix")
heatmap_mse_covariance <- create_heatmap(statistics_em$MSE_covariance, title = "Heatmap of MSE Covariance Matrix")

# Create heatmaps for Covariance_matrix_beta (from statistics_beta)
heatmap_average_covariance_beta <- create_heatmap(statistics_beta$average_covariance, title = "Heatmap of Average Covariance Matrix - beta")
heatmap_bias_covariance_beta <- create_heatmap(statistics_beta$bias_covariance, title = "Heatmap of Bias Covariance Matrix - beta")
heatmap_mse_covariance_beta <- create_heatmap(statistics_beta$MSE_covariance, title = "Heatmap of MSE Covariance Matrix - beta")

# Print heatmaps for Covariance_matrix (from statistics_em)
print(heatmap_true_covariance)
print(heatmap_average_covariance)
print(heatmap_bias_covariance)
print(heatmap_mse_covariance)

# Print heatmaps for Covariance_matrix_beta (from statistics_beta)
print(heatmap_average_covariance_beta)
print(heatmap_bias_covariance_beta)
print(heatmap_mse_covariance_beta)



rm(list = ls())
source("Scripts/Analysis/functions.R")
source("Scripts/Analysis/loading-objects.R")

all_analysis_results <- readRDS("all_analysis_results.rds")

# Load the relevant dataframe
heatmap_dataframe <- all_analysis_results$`overlap2-large-weak`

# Calculate statistics for both em and em_beta
statistics_em <- calculate_statistics(heatmap_dataframe, type = "em")
statistics_beta <- calculate_statistics(heatmap_dataframe, type = "em_beta")

# Create heatmaps for B_permuted (from statistics_em)
create_factor_heatmap(statistics_em$B_true, title = "True factors")
create_factor_heatmap(statistics_em$average_matrix, title = "Heatmap of average factors")
create_factor_heatmap(statistics_em$bias_matrix, title = "Heatmap of bias factors")
create_factor_heatmap(statistics_em$MSE_matrix, title = "Heatmap of MSE factors")
stop()

# Create heatmaps for B_permuted_beta (from statistics_beta)
create_factor_heatmap(statistics_beta$average_matrix, title = "Heatmap of average factors - beta")
create_factor_heatmap(statistics_beta$bias_matrix, title = "Heatmap of bias factors - beta")
create_factor_heatmap(statistics_beta$MSE_matrix, title = "Heatmap of MSE factors - beta")

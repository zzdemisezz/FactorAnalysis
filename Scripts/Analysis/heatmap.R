library(ggplot2)
library(reshape2)

# Function to create a heatmap with the correct orientation
create_heatmap <- function(matrix_data, title = "Heatmap", x_label = "Factors", y_label = "Loadings") {
  
  # Convert the matrix to a format suitable for ggplot2
  matrix_melted <- melt(matrix_data)
  
  # Check if the melt was successful
  if (!all(c("Var1", "Var2", "value") %in% colnames(matrix_melted))) {
    stop("The melted data frame does not contain the expected 'Var1', 'Var2', and 'value' columns.")
  }
  
  # Create the heatmap with the y-axis reversed
  heatmap_plot <- ggplot(matrix_melted, aes(x = Var2, y = Var1, fill = value)) +
    geom_tile() +
    scale_y_reverse() +  # Reverse the y-axis
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
    theme_minimal() +
    labs(title = title, x = x_label, y = y_label)
  
  # Return the plot object
  return(heatmap_plot)
}
# Define a function to generate and print heatmaps
generate_heatmaps <- function(true_matrix, statistics, prefix) {
  if (grepl("Covariance", prefix)) {
    heatmap_true <- create_heatmap(true_matrix, title = paste("Heatmap of true", prefix))
    heatmap_average <- create_heatmap(statistics$average_covariance, title = paste("Heatmap of average", prefix))
    heatmap_bias <- create_heatmap(statistics$bias_covariance, title = paste("Heatmap of bias", prefix))
    heatmap_variance <- create_heatmap(statistics$variance_covariance, title = paste("Heatmap of variance", prefix))
    heatmap_MSE <- create_heatmap(statistics$MSE_covariance, title = paste("Heatmap of MSE", prefix))
  } else {
    heatmap_true <- create_heatmap(true_matrix, title = paste("Heatmap of true", prefix))
    heatmap_average <- create_heatmap(statistics$average_matrix, title = paste("Heatmap of average", prefix))
    heatmap_bias <- create_heatmap(statistics$bias_matrix, title = paste("Heatmap of bias", prefix))
    heatmap_variance <- create_heatmap(statistics$variance_matrix, title = paste("Heatmap of variance", prefix))
    heatmap_MSE <- create_heatmap(statistics$MSE_matrix, title = paste("Heatmap of MSE", prefix))
  }
  
  # Print all heatmaps
  print(heatmap_true)
  print(heatmap_average)
  print(heatmap_bias)
  print(heatmap_variance)
  print(heatmap_MSE)
}

# Calculate statistic matrices
calculate_average_matrix <- function(matrix_list) {
  Reduce(`+`, matrix_list) / length(matrix_list)
}
calculate_variance <- function(matrix_list) {
  # Calculate the variance for each element across the list of matrices
  elementwise_variance <- Reduce(`+`, lapply(matrix_list, function(x) (x - calculate_average_matrix(matrix_list))^2)) / (length(matrix_list) - 1)
  return(elementwise_variance)
}
calculate_MSE <- function(true_matrix, matrix_list) {
  mse_matrix <- Reduce(`+`, lapply(matrix_list, function(matrix) {
    (true_matrix - matrix)^2
  })) / length(matrix_list)
  
  return(mse_matrix)
}
# Calculate all statistic matrices for model
calculate_statistics <- function(dataframe, type = "em") {
  # Retrieve B_true and Covariance_true from the dataframe
  B_true <- dataframe$B_True[[1]]
  Covariance_true <- dataframe$Covariance_matrix_true[[1]]
  
  # Select appropriate matrices based on the type
  if (type == "em") {
    permuted_matrices <- dataframe$B_permuted
    covariance_matrices <- dataframe$Covariance_matrix
  } else if (type == "em_beta") {
    permuted_matrices <- dataframe$B_permuted_beta
    covariance_matrices <- dataframe$Covariance_matrix_beta
  } else {
    stop("Invalid type specified. Choose 'em' or 'em_beta'.")
  }
  
  # For B matrices
  average_matrix <- calculate_average_matrix(permuted_matrices)
  bias_matrix <- average_matrix - B_true
  variance_matrix <- calculate_variance(permuted_matrices)
  MSE_matrix <- calculate_MSE(B_true, permuted_matrices)
  
  # For Covariance matrices
  average_covariance <- calculate_average_matrix(covariance_matrices)
  bias_covariance <- average_covariance - Covariance_true
  variance_covariance <- calculate_variance(covariance_matrices)
  MSE_covariance <- calculate_MSE(Covariance_true, covariance_matrices)
  
  list(
    average_matrix = average_matrix,
    bias_matrix = bias_matrix,
    variance_matrix = variance_matrix,
    MSE_matrix = MSE_matrix,
    average_covariance = average_covariance,
    bias_covariance = bias_covariance,
    variance_covariance = variance_covariance,
    MSE_covariance = MSE_covariance
  )
}
# Calculate metrics
calculate_metrics <- function(bias, variance, mse) {
  c(Mean_Bias = mean(bias), Mean_Variance = mean(variance), Mean_MSE = mean(mse))
}

# Function to see how often we get structure right
compare_GAMMA_to_B_true <- function(dataframe, type = "em") {
  if (type == "em") {
    GAMMA_permuted_list <- dataframe$GAMMA_permuted
  } else if (type == "em_beta") {
    GAMMA_permuted_list <- dataframe$GAMMA_permuted_beta
  } else {
    stop("Invalid type specified. Choose 'em' or 'em_beta'.")
  }
  
  B_true <- dataframe$B_True[[1]]  # Assuming B_true is the same across simulations
  
  # Compare each GAMMA_permuted matrix with B_true and get the indices where they don't match
  match_results <- sapply(GAMMA_permuted_list, function(GAMMA_permuted) {
    all.equal(GAMMA_permuted, B_true)
  })
  
  # Get indices where the matrices do not match
  non_matching_indices <- which(match_results != TRUE)
  
  # Calculate the number of exact matches
  exact_matches <- length(match_results) - length(non_matching_indices)
  
  return(list(
    exact_matches = exact_matches,
    non_matching_indices = non_matching_indices
  ))
}






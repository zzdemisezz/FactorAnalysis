# Function to run simulations for a specific data generator
run_simulations <- function(generator, num_simulations) {
  all_datasets <- vector("list", num_simulations)
  all_results_em <- vector("list", num_simulations)
  all_results_em_beta <- vector("list", num_simulations)
  all_results_em_pxl <- vector("list", num_simulations)  # Store results for em_pxl
  pca_best_count_em <- 0  # Initialize counter for PCA best results in em
  pca_best_count_em_beta <- 0  # Initialize counter for PCA best results in em_beta
  pca_best_count_em_pxl <- 0  # Initialize counter for PCA best results in em_pxl
  total_simulation_time <- 0  # Initialize total simulation time
  
  total_simulation_time <- system.time({
    for (sim in 1:num_simulations) {
      print(paste("Simulation", sim))
      
      # Generate new data for each simulation run
      data <- generator()  # Call the generator function to get the data
      all_datasets[[sim]] <- data  # Save the generated dataset
      
      # Run the EM algorithm
      results_em <- run_em_algorithm(em, data, q, dim1, dim2, tol = tol, 
                                     max_iter = max_iter, ll = ll, num_runs = num_runs)
      results_em_beta <- run_em_algorithm(em_beta, data, q, tol = tol, max_iter = max_iter, 
                                          ll = ll, num_runs = num_runs)
      results_em_pxl <- run_em_algorithm(em_pxl, data, q, dim1, dim2, tol = tol, 
                                         max_iter = max_iter, ll = ll, num_runs = num_runs)  # Run em_pxl
      
      pca_best_count_em <- pca_best_count_em + results_em$pca_best
      pca_best_count_em_beta <- pca_best_count_em_beta + results_em_beta$pca_best
      pca_best_count_em_pxl <- pca_best_count_em_pxl + results_em_pxl$pca_best  # Update for em_pxl
      
      # Save the results for each simulation
      all_results_em[[sim]] <- results_em
      all_results_em_beta[[sim]] <- results_em_beta
      all_results_em_pxl[[sim]] <- results_em_pxl  # Save em_pxl results
    }
  })[3]  # The third element of the system.time() result is the elapsed time
  
  return(list(
    all_results_em = all_results_em,
    all_results_em_beta = all_results_em_beta,
    all_results_em_pxl = all_results_em_pxl,  # Return em_pxl results
    all_datasets = all_datasets,  # Return the list of datasets
    pca_best_count_em = pca_best_count_em,
    pca_best_count_em_beta = pca_best_count_em_beta,
    pca_best_count_em_pxl = pca_best_count_em_pxl,  # Return pca_best count for em_pxl
    total_simulation_time = total_simulation_time
  ))
}

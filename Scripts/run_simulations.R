# Function to run simulations for a specific data generator
run_simulations <- function(generator, num_simulations) {
  all_results_em <- vector("list", num_simulations)
  all_results_em_beta <- vector("list", num_simulations)
  pca_best_count_em <- 0  # Initialize counter for PCA best results in em
  pca_best_count_em_beta <- 0  # Initialize counter for PCA best results in em_beta
  total_simulation_time <- 0  # Initialize total simulation time
  
  total_simulation_time <- system.time({
    for (sim in 1:num_simulations) {
      print(paste("Simulation", sim))
      
      # Generate new data for each simulation run
      data <- generator()  # Call the generator function to get the data
      
      # Run the EM algorithm
      results_em <- run_em_algorithm(em, data, q, dim1, dim2, tol = tol, 
                                     max_iter = max_iter, ll = ll, num_runs = num_runs)
      results_em_beta <- run_em_algorithm(em_beta, data, q, tol = tol, max_iter = max_iter, 
                                          ll = ll, num_runs = num_runs)
      
      pca_best_count_em <- pca_best_count_em + results_em$pca_best
      pca_best_count_em_beta <- pca_best_count_em_beta + results_em_beta$pca_best
      
      # Save the results for each simulation
      all_results_em[[sim]] <- results_em
      all_results_em_beta[[sim]] <- results_em_beta
    }
  })[3]  # The third element of the system.time() result is the elapsed time
  
  return(list(
    all_results_em = all_results_em,
    all_results_em_beta = all_results_em_beta,
    pca_best_count_em = pca_best_count_em,
    pca_best_count_em_beta = pca_best_count_em_beta,
    total_simulation_time = total_simulation_time
  ))
}
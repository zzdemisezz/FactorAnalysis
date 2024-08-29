# Load necessary scripts and libraries
source("Scripts/data_generation.R")
source("Scripts/em.R")
source("Scripts/em_beta.R")
source("Scripts/run_em.R")
source("Scripts/run_simulations.R")

# Command line arguments
args <- commandArgs(trailingOnly = TRUE)
data_generator_name <- args[1]  # Name of the data generator
simulation_index <- as.integer(args[2])  # Index of the simulation

# Parameters
n <- 500 
q <- 3
dim1 <- 10
dim2 <- 10
print_factors <- FALSE
max_iter <- 5000
tol <- 1e-2
ll <- FALSE
num_runs <- 5

# List of datasets to generate and their descriptions
data_generators <- list(
  list(name = "SmallFactors_3x3_Strong", generator = function() generate_data(n, dim1, dim2, q, 3, corr = "strong", print_factors = print_factors)),
  list(name = "SmallFactors_3x3_Moderate", generator = function() generate_data(n, dim1, dim2, q, 3, corr = "moderate", print_factors = print_factors)),
  list(name = "SmallFactors_3x3_Weak", generator = function() generate_data(n, dim1, dim2, q, 3, corr = "weak", print_factors = print_factors)),
  
  list(name = "LargeFactors_5x5_Strong", generator = function() generate_data(n, dim1, dim2, q, 5, corr = "strong", print_factors = print_factors)),
  list(name = "LargeFactors_5x5_Moderate", generator = function() generate_data(n, dim1, dim2, q, 5, corr = "moderate", print_factors = print_factors)),
  list(name = "LargeFactors_5x5_Weak", generator = function() generate_data(n, dim1, dim2, q, 5, corr = "weak", print_factors = print_factors)),
  
  list(name = "Overlap2_Small_Strong", generator = function() generate_data_2overlap(n, dim1, dim2, q, overlap = "small", corr = "strong", print_factors = print_factors)),
  list(name = "Overlap2_Small_Moderate", generator = function() generate_data_2overlap(n, dim1, dim2, q, overlap = "small", corr = "moderate", print_factors = print_factors)),
  list(name = "Overlap2_Small_Weak", generator = function() generate_data_2overlap(n, dim1, dim2, q, overlap = "small", corr = "weak", print_factors = print_factors)),
  
  list(name = "Overlap2_Large_Strong", generator = function() generate_data_2overlap(n, dim1, dim2, q, overlap = "big", corr = "strong", print_factors = print_factors)),
  list(name = "Overlap2_Large_Moderate", generator = function() generate_data_2overlap(n, dim1, dim2, q, overlap = "big", corr = "moderate", print_factors = print_factors)),
  list(name = "Overlap2_Large_Weak", generator = function() generate_data_2overlap(n, dim1, dim2, q, overlap = "big", corr = "weak", print_factors = print_factors)),
  
  list(name = "Overlap3_Small_Strong", generator = function() generate_data_3overlap(n, dim1, dim2, q, overlap = "small", corr = "strong", print_factors = print_factors)),
  list(name = "Overlap3_Small_Moderate", generator = function() generate_data_3overlap(n, dim1, dim2, q, overlap = "small", corr = "moderate", print_factors = print_factors)),
  list(name = "Overlap3_Small_Weak", generator = function() generate_data_3overlap(n, dim1, dim2, q, overlap = "small", corr = "weak", print_factors = print_factors)),
  
  list(name = "Overlap3_Large_Strong", generator = function() generate_data_3overlap(n, dim1, dim2, q, overlap = "big", corr = "strong", print_factors = print_factors)),
  list(name = "Overlap3_Large_Moderate", generator = function() generate_data_3overlap(n, dim1, dim2, q, overlap = "big", corr = "moderate", print_factors = print_factors)),
  list(name = "Overlap3_Large_Weak", generator = function() generate_data_3overlap(n, dim1, dim2, q, overlap = "big", corr = "weak", print_factors = print_factors))
)

# Find the right generator
generator <- NULL
for (gen in data_generators) {
  if (gen$name == data_generator_name) {
    generator <- gen$generator
    break
  }
}

if (is.null(generator)) {
  stop("Invalid data generator name.")
}

# Run the simulation
set.seed(12 + simulation_index)  # Ensure different seeds for different runs
results <- run_simulations(generator, 1)


# Create subfolder if it doesn't exist
output_dir <- file.path("/well/nichols/users/rht383/results", data_generator_name)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save the results to a file in the subfolder
output_file <- file.path(output_dir, paste0("results_sim_", simulation_index, ".rds"))
saveRDS(results, file = output_file)

# Check if the 'mvtnorm' package is installed
if (!requireNamespace("mvtnorm", quietly = TRUE)) {
  install.packages("Scripts/mvtnorm_1.2-6.tar.gz", repos = NULL, type="source")
}

library(mvtnorm)

# Function to generate B, PSI, and Y for 2D data 
generate_data <- function(n, dim1, dim2, q, square_size = 5, 
                          corr = c("weak", "moderate", "strong"), 
                          print_factors = FALSE) {
  # Number of parameters
  p <- dim1 * dim2
  
  # Match the correlation argument
  corr <- match.arg(corr)
  
  # Set the PSI value range based on the correlation type
  psi_range <- switch(corr,
                      "weak" = c(2.5, 7.5),
                      "moderate" = c(0.5, 1.5),
                      "strong" = c(0.05, 0.15))
  
  # Set dimensions and initialize the B_factors array
  B_factors <- array(0, dim = c(dim1, dim2, q))
  
  # Define square positions based on square_size
    if (square_size == 3) {
    B_factors[1:3, 1:3, 1] <- 1        # Top left
    B_factors[8:10, 4:6, 2] <- 1       # Bottom middle
    B_factors[1:3, 8:10, 3] <- 1       # Top right
  } else if (square_size == 5) {
    B_factors[1:5, 1:5, 1] <- 1        # Top left
    B_factors[6:10, 3:7, 2] <- 1       # Bottom middle
    B_factors[1:5, 6:10, 3] <- 1       # Top right
  } else {
    stop("Invalid square_size. Choose either 3 or 5.")
  }
  
  if (print_factors == TRUE) {
    print(B_factors[, , 1])
    print(B_factors[, , 2])
    print(B_factors[, , 3])
  }
  
  # Convert slices to column vectors and combine into a matrix
  B_true <- do.call(cbind, lapply(1:q, function(i) 
    as.vector(B_factors[, , i])))
  
  # Set a specific seed for generating PSI_true
  old_seed <- .Random.seed  # Save the current random seed
  set.seed(12)  # Set a seed specifically for generating PSI_true
  PSI_true <- diag(runif(p, min = psi_range[1], max = psi_range[2]))
  .Random.seed <<- old_seed  # Restore the previous random seed
  
  # Generate noise covariance matrix and synthetic data
  Covariance_matrix_true <- B_true %*% t(B_true) + PSI_true
  Y <- rmvnorm(n, mean = rep(0, p), sigma = Covariance_matrix_true)
  
  cor_matrix <- cov2cor(Covariance_matrix_true)
  
  return(list(Y = Y, B_true = B_true, PSI_true = PSI_true, 
              B_factors = B_factors, Covariance_matrix_true = Covariance_matrix_true, 
              cor_matrix = cor_matrix))
}

# Function to generate B, PSI, and Y for 2D data with slightly overlapping squares
generate_data_2overlap <- function(n, dim1, dim2, q, overlap = "small", 
                                   corr = c("weak", "moderate", "strong"), 
                                   print_factors = FALSE) {
  # Number of parameters
  p <- dim1 * dim2
  
  # Match the correlation argument
  corr <- match.arg(corr)
  
  # Set the PSI value range based on the correlation type
  psi_range <- switch(corr,
                      "weak" = c(2.5, 7.5),
                      "moderate" = c(0.5, 1.5),
                      "strong" = c(0.05, 0.15))
  
  # Set dimensions and initialize the B_factors array
  B_factors <- array(0, dim = c(dim1, dim2, q))
  
  # Define overlapping squares based on overlap
  if (overlap == "small") {
    B_factors[1:4, 1:4, 1] <- 1        # Top left
    B_factors[3:6, 3:6, 2] <- 1        # Overlapping with first square
    B_factors[1:4, 7:10, 3] <- 1       # Top right
  } else if (overlap == "big") {
    B_factors[1:10, 1:4, 1] <- 1       # Top left
    B_factors[6:10, 1:10, 2] <- 1      # Overlapping with first square
    B_factors[1:4, 7:10, 3] <- 1       # Top right
  } else {
    stop("Invalid overlap size. Choose either 'small' or 'big'.")
  }
  
  if (print_factors == TRUE) {
    print(B_factors[, , 1])
    print(B_factors[, , 2])
    print(B_factors[, , 3])
  }
  
  # Convert slices to column vectors and combine into a matrix
  B_true <- do.call(cbind, lapply(1:q, function(i) 
    as.vector(B_factors[, , i])))
  
  # Set a specific seed for generating PSI_true
  old_seed <- .Random.seed  # Save the current random seed
  set.seed(12)  # Set a seed specifically for generating PSI_true
  PSI_true <- diag(runif(p, min = psi_range[1], max = psi_range[2]))
  .Random.seed <<- old_seed  # Restore the previous random seed
  
  # Generate noise covariance matrix and synthetic data
  Covariance_matrix_true <- B_true %*% t(B_true) + PSI_true
  Y <- rmvnorm(n, mean = rep(0, p), sigma = Covariance_matrix_true)
  
  cor_matrix <- cov2cor(Covariance_matrix_true)
  
  return(list(Y = Y, B_true = B_true, PSI_true = PSI_true, 
              B_factors = B_factors, Covariance_matrix_true = Covariance_matrix_true, 
              cor_matrix = cor_matrix))
}


# Function to generate B, PSI, and Y for 2D data with slightly overlapping squares
generate_data_3overlap <- function(n, dim1, dim2, q, overlap = "small", 
                                   corr = c("weak", "moderate", "strong"), 
                                   print_factors = FALSE) {
  # Number of parameters
  p <- dim1 * dim2
  
  # Match the correlation argument
  corr <- match.arg(corr)
  
  # Set the PSI value range based on the correlation type
  psi_range <- switch(corr,
                      "weak" = c(2.5, 7.5),
                      "moderate" = c(0.5, 1.5),
                      "strong" = c(0.05, 0.15))
  
  # Set dimensions and initialize the B_factors array
  B_factors <- array(0, dim = c(dim1, dim2, q))
  
  # Define overlapping squares based on overlap
  if (overlap == "small") {
    B_factors[1:5, 1:5, 1] <- 1        # Top left
    B_factors[4:7, 4:7, 2] <- 1        # Overlapping with first square
    B_factors[6:10, 6:10, 3] <- 1      # Top right
  } else if (overlap == "big") {
    B_factors[1:5, 1:5, 1] <- 1        # Top left
    B_factors[3:9, 3:5, 2] <- 1        # Overlapping with first square
    B_factors[1:3, 3:9, 3] <- 1        # Top right
  } else {
    stop("Invalid overlap size. Choose either 'small' or 'big'.")
  }
  
  if (print_factors == TRUE) {
    print(B_factors[, , 1])
    print(B_factors[, , 2])
    print(B_factors[, , 3])
  }
  
  # Convert slices to column vectors and combine into a matrix
  B_true <- do.call(cbind, lapply(1:q, function(i) 
    as.vector(B_factors[, , i])))
  
  # Set a specific seed for generating PSI_true
  old_seed <- .Random.seed  # Save the current random seed
  set.seed(12)  # Set a seed specifically for generating PSI_true
  PSI_true <- diag(runif(p, min = psi_range[1], max = psi_range[2]))
  .Random.seed <<- old_seed  # Restore the previous random seed
  
  # Generate noise covariance matrix and synthetic data
  Covariance_matrix_true <- B_true %*% t(B_true) + PSI_true
  Y <- rmvnorm(n, mean = rep(0, p), sigma = Covariance_matrix_true)
  
  cor_matrix <- cov2cor(Covariance_matrix_true)
  
  return(list(Y = Y, B_true = B_true, PSI_true = PSI_true, 
              B_factors = B_factors, Covariance_matrix_true = Covariance_matrix_true, 
              cor_matrix = cor_matrix))
}



source("Scripts/data_generation.R")

# initialisation
set.seed(12)
n <- 200
dim1 <- 10
dim2 <- 10
q <- 3

data <- generate_data(n, dim1, dim2, q, 4, "weak", FALSE)

# data <- generate_data_2overlap(n, dim1, dim2, q, overlap = "big",
#                          print_factors = TRUE)
# 
# data <- generate_data_3overlap(n, dim1, dim2, q, overlap = "big",
#                                  print_factors = TRUE)
# 
# data <- generate_data_psi(n, dim1, dim2, q, square_size = 4, scale = 10,
#                          print_factors = TRUE)
# 
# # Vertical gradient
# result_vertical <- generate_data_vertical_gradient(n = 100, dim1 = 10,
#                                                    dim2 = 10, q = 3,
#                                                    print_factors = TRUE)


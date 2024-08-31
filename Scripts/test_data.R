source("Scripts/data_generation.R")

# initialisation
n <- 200
dim1 <- 10
dim2 <- 10
q <- 3

set.seed(13)
data <- generate_data(n, dim1, dim2, q, 5, "weak", FALSE)
set.seed(14)
data2 <- generate_data(n, dim1, dim2, q, 5, "weak", FALSE)
identical(data$PSI_true, data2$PSI_true)
identical(data$Y, data2$Y)

View(data$PSI_true)
View(data2$PSI_true)
View(data$Y)
View(data2$Y)

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


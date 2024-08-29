library(clue)
# Assuming B_true and B_est are both q x p matrices (same dimensions)
cost_matrix <- as.matrix(dist(rbind(t(data$B_true), t(best_result2$GAMMA_truncated))))[1:q, (q+1):(2*q)]

# Solve the assignment problem to find the best permutation
perm <- solve_LSAP(cost_matrix)

# Permute the columns of B_est according to the optimal permutation
B_est_permuted <- best_result2$B_truncated[, perm]

# Adjust signs
for (i in 1:ncol(B_est_permuted)) {
  if (cor(data$B_true[, i], B_est_permuted[, i]) < 0) {
    B_est_permuted[, i] <- -B_est_permuted[, i]
  }
}

View(B_est_permuted)
View(data$B_true)

# Now compare the aligned and sign-adjusted B_est_permuted with B_true
rmse <- sqrt(mean((data$B_true - B_est_permuted)^2))
cat("RMSE after permutation and sign adjustment:", rmse, "\n")

#GAMMA ####
cost_matrix <- as.matrix(dist(rbind(t(data$B_true), t(best_result$GAMMA_truncated))))[1:q, (q+1):(2*q)]

# Solve the assignment problem to find the best permutation
perm <- solve_LSAP(cost_matrix)

# Permute the columns of B_est according to the optimal permutation
GAMMA_est_permuted <- best_result$GAMMA_truncated[, perm]
View(GAMMA_est_permuted)
identical(GAMMA_est_permuted, data$B_true)

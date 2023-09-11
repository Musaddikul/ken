rm(list = ls())

library(MASS)

num_datasets <- 1000
train_sample_size <- 100
test_sample_size <- 100
d <- 50
a_values <- c(9, 15)
correlation_values <- c(0, 0.5)
signal_to_noise_ratio <- 2
contamination_prob <- 0.003

# Function to generate data
set.seed(123)
generate_data <- function(a, corr) {
  non_zero_coeffs <- 
  X <- mvrnorm(n = train_sample_size + test_sample_size, rep(0, d), diag(d))
  Y <- X[, 1:a] %*% rep(c(7, 6, 5), length.out = a) + rnorm(train_sample_size, sd = sqrt(sum(c(7,6,5)^2) / signal_to_noise_ratio))
  return(list(X = X, Y = Y))
}


introduce_contamination <- function(X, Y) {
  num_contaminated_rows <- round(contamination_prob * train_sample_size)
  contaminated_indices <- sample(seq_len(train_sample_size), num_contaminated_rows, replace = FALSE)
  X[contaminated_indices, ] <- matrix(rnorm(num_contaminated_rows * d), ncol = d)
  Y[contaminated_indices] <- rnorm(num_contaminated_rows, sd = sqrt(sum(Y^2) / signal_to_noise_ratio))
  return(list(X = X, Y = Y))
}


results <- list()

for (a_val in a_values) {
  for (corr_val in correlation_values) {
    for (dataset in 1:num_datasets) {
      data <- generate_data(a_val, corr_val)
      contaminated_data <- introduce_contamination(data$X, data$Y)
      df <- data.frame(cbind(contaminated_data$X, contaminated_data$Y))
      
      stepwise_model <- lm(contaminated_data$Y ~ ., data = df)
      stepwise_selected <- step(stepwise_model, direction = "both")
      stepwise_formula = formula(stepwise_selected)
      
      forward_model <- lm(contaminated_data$Y ~ 1, data = df)
      forward_selected <- step(forward_model, scope = list(upper = ~.), direction = "forward")
      forward_formula = formula(forward_selected)
      
      return(list(Stepwise_Model = stepwise_formula, Forward_Model = forward_formula))
    }
  }
}

print(stepwise_formula)
print(forward_formula)

#=============================THE END==============================#
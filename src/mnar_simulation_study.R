
source("./src/al_4_var_functions.R") # to be loaded if done with 4 features
source("./src/al_5_var_functions.R") # to be loaded if done with 5 features

# Example usage 2 variables from the Multivariate Gaussian Distribution:
cov_g = 0.2
cov_b = -0.2

mu_g <- c(0, 0)
mu_b <- c(2, 2)
mu_g_list = list(c(0, 0))
mu_b_list = list(c(2, 2))
sigma_g_list <- list(c(1, cov_g, cov_g, 1))
sigma_b_list <- list(c(1, cov_b, cov_b, 1))
miss_g = c(0.6)
miss_b = c(0.9)
phi = c(1)
b <- 0.03
n_train = c (50, 300)
n_test = c (50000)
m = c(10)

# Example usage 3 variables from the Multivariate Gaussian Distribution:
cov_g = 0.2
cov_b = -0.2

mu_g_list = list(c(0,0,0))
mu_b_list = list(c(2,1,1))
sigma_g_list = list(c(1, cov_g, cov_g, cov_g, 1, cov_g, cov_g, cov_g, 1))
sigma_b_list = list(c(1, cov_b, cov_b, cov_b, 1, cov_b, cov_b, cov_b, 1))
m = c(10)
n_train = c (50, 300)
n_test = c (50000)
miss_g = c(0.6)
miss_b = c(0.9)
phi = c(1)
b <- c(0.3)


# Create a list of variables
variable_list <- list(mu_g_list = mu_g_list, sigma_g_list = sigma_g_list, sigma_b_list = sigma_b_list,
                      mu_b_list = mu_b_list, m = m, 
                      n_train = n_train, n_test = n_test, miss_g = miss_g, miss_b = miss_b, 
                      phi = phi)

# Generate all possible combinations of variables
combinations <- expand.grid(variable_list)


for (i in 1:nrow(combinations)) {
  # Extract current combination
  current_combination <- combinations[i, ]
  
  ###Actual code
  
  #Example usage 3 variables:
  #mu_g <-c(1, 0.5, 0.2)
  mu_g <- current_combination$mu_g[[1]] #c(1, 0.5, 0.2)
  mu_b <- current_combination$mu_b[[1]] #c(2, 1, 1)
  
  if (length(current_combination$mu_g[[1]]) == 3) {
  sigma_g <- matrix(current_combination$sigma_g_list[[1]], nrow = 3)
  sigma_b <- matrix(current_combination$sigma_b_list[[1]], nrow = 3)
  }else{
    sigma_g <- matrix(current_combination$sigma_g_list[[1]], nrow = 2)
    sigma_b <- matrix(current_combination$sigma_b_list[[1]], nrow = 2)
    
  }
  
  n_train <- current_combination$n_train
  n_test <- current_combination$n_test
  miss_g <- current_combination$miss_g
  miss_b <- current_combination$miss_b
  
  generated_data <- generate(mu_g, mu_b, sigma_g, sigma_b, b, n_train)
  generated_data_full <- generated_data
  generated_data <- missingness_mnar(generated_data, miss_g, miss_b)
  
  test_data <- generate(mu_g, mu_b, sigma_g, sigma_b, b, n_test)
  
  #no point in creating missingness in the test data
  #test_data <- missingness_mnar(test_data, miss_b, miss_g)
  
  #Galimard
  train_data_gal = generated_data
  test_data_gal = test_data
  
  #train_data_gal = generated_data[-c(4,6)]
  #test_data_gal = test_data
  #pred_test = galimard_option_2(train_data_gal, "Y_hat", current_combination[[3]] , test_data_gal)
  
  
  # PPMA paper 
  train_data_ppma = generated_data
  test_data_ppma = test_data
  
  #train_data_ppma  = generated_data[-c(3,4,6)]
  #test_data_ppma = test_data
  #bla = PPMA_MI(train_data_ppma, 'Y_hat', current_combination[[3]] , test_data_ppma)
  
  #train_data_ppma_3_vars  = generated_data[-c(4,6)]
  
  
  # Run each function and store its result and parameters in the dataframe
  if (i ==1) {
    results_df <- data.frame(dummy = 1)
    # Run each function and store its result and parameters in the dataframe
    
    results_df$mean_good <- paste(mu_g, collapse = ", ")
    results_df$mean_bad <- paste(mu_b, collapse = ", ")
    results_df$covariance_good <- paste(sigma_g, collapse = ", ")
    results_df$covariance_bad <- paste(sigma_b, collapse = ", ")
    results_df$miss_good <- miss_g
    results_df$miss_bad <- miss_b
    results_df$n_train <- n_train
    results_df$n_test <- n_test
    results_df$phi <- phi
    results_df$auc_gal <- galimard_option_2(train_data_gal, "Y",current_combination$m , test_data_gal)
    results_df$auc_ppma <- PPMA_MI_sim(train_data_ppma, 'Y', current_combination$m, test_data_ppma, current_combination$phi)
    results_df$auc_cca <- complete_case_func(generated_data, 'Y', test_data)
    results_df$oracle_auc <- oracle_model_func(generated_data_full, "Y", test_data)
    
    
  } else {
    
    new_results <- data.frame(dummy = 1)
    # Run each function and store its result and parameters in the dataframe
    
    new_results$mean_good <- paste(mu_g, collapse = ", ")
    new_results$mean_bad <- paste(mu_b, collapse = ", ")
    new_results$covariance_good <- paste(sigma_g, collapse = ", ")
    new_results$covariance_bad <- paste(sigma_b, collapse = ", ")
    new_results$miss_good <- miss_g
    new_results$miss_bad <- miss_b
    new_results$n_train <- n_train
    new_results$n_test <- n_test
    new_results$phi <- phi
    new_results$auc_gal <- galimard_option_2(train_data_gal, "Y", current_combination$m, test_data_gal)
    new_results$auc_ppma <- PPMA_MI_sim(train_data_ppma, 'Y', current_combination$m, test_data_ppma, current_combination$phi)
    new_results$auc_cca <- complete_case_func(generated_data, 'Y', test_data)
    #PPMA_MI_3_var(train_data_ppma_3_vars, 'Y_hat', current_combination$m, test_data_ppma)
    new_results$oracle_auc <- oracle_model_func(generated_data_full, "Y", test_data)
    results_df <- rbind(results_df, new_results)
    
  }
  
} 

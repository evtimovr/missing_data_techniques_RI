
require(MASS)
require(mvtnorm)
require(Metrics)
require(scoringTools)

source("./src/al_5_var_functions.R")

#Latest used data
cov_g = 0.2
cov_b = -0.2

mu_g_list = list(c(1.2,0.6,0.6), c(1.5, 0.8, 0.8), c(1,0.5,0.5))
mu_b_list = list(c(2,1,1))
sigma_g_list = list(c(1, cov_g, cov_g, cov_g, 1, cov_g, cov_g, cov_g, 1))
sigma_b_list = list(c(1, cov_b, cov_b, cov_b, 1, cov_b, cov_b, cov_b, 1),c(1, cov_g, cov_g, cov_g, 1, cov_g, cov_g, cov_g, 1))
m = c(10)
n_train = c (300)
n_test = c (30000)
miss_g = c(0.6)
miss_b = c(0.9)
phi = c(0.5)
j = c (100)
alpha = c (0.1,0.2,0.3)


# Create a list of variables
variable_list <- list(mu_g_list = mu_g_list, sigma_g_list = sigma_g_list, sigma_b_list = sigma_b_list,
                      mu_b_list = mu_b_list, #m = m, 
                      n_train = n_train, n_test = n_test, b = b, 
                      phi = phi, j = j, alpha = alpha)

# Generate all possible combinations of variables
combinations <- expand.grid(variable_list)


sim_df = data.frame(combination = numeric(), 
                    iteration = numeric(),
                    auc_a = numeric(),
                    auc_o = numeric(), 
                    auc_gal = numeric(),
                    auc_ppma = numeric(), 
                    auc_aug = numeric())
df_results <- list()
for (i in 1:nrow(combinations)) {
  # Extract current combination
  current_combination <- combinations[i, ]
  
  mu_g <- current_combination$mu_g[[1]] #c(1, 0.5, 0.2)
  mu_b <- current_combination$mu_b[[1]] #c(2, 1, 1)
  sigma_g <- matrix(current_combination$sigma_g_list[[1]], nrow = 3)
  sigma_b <- matrix(current_combination$sigma_b_list[[1]], nrow = 3)
  b <- current_combination$b
  j <- current_combination$j
  n_train <- current_combination$n_train
  n_test <- current_combination$n_test
  alpha <- current_combination$alpha
  
  df_results[[i]] = simulation_gal(mu_g, mu_b, sigma_g, sigma_b, n_train, n_test, b, alpha, j, i)
}

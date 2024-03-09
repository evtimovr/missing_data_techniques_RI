require(MASS)
require(mvtnorm)
require(Metrics)
require(mice)
require(scoringTools)

source("./src/al_4_var_functions.R")
combinations <- read.csv("./params/combinations_4.csv")

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
  
  mu_g <- as.numeric(strsplit(current_combination$mu_g, ",")[[1]]) #c(1, 0.5, 0.2)
  mu_b <- as.numeric(strsplit(current_combination$mu_b, ",")[[1]]) #c(2, 1, 1)
  sigma_g <- matrix(as.numeric(strsplit(current_combination$sigma_g_list, ",")[[1]]), nrow = 2)
  sigma_b <- matrix(as.numeric(strsplit(current_combination$sigma_b_list, ",")[[1]]), nrow = 2)
  b <- current_combination$b
  j <- current_combination$j
  n_train <- current_combination$n_train
  n_test <- current_combination$n_test
  alpha <- current_combination$alpha
  
  df_results[[i]] = simulation_gal(mu_g, mu_b, sigma_g, sigma_b, n_train, n_test, b, alpha, j, i)
}

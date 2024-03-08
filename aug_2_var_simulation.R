require(MASS)
require(mvtnorm)

simulation_gal <- function(mu_g, mu_b, sigma_g, sigma_b, n_train, n_test, b, alpha, j_max, i) {
  # Generate initial dataset D_star
  require(Metrics)
  require(scoringTools)
  set.seed(423)
  D_star <- generate(mu_g, mu_b, sigma_g, sigma_b, b, n_train)
  
  # Generate dataset D
  D <- generate(mu_g, mu_b, sigma_g, sigma_b, b, n_test)
  
  # Calculate threshold teta
  # alpha is the acceptance rate
  teta <- quantile(D_star[,1], 1 - alpha)
  
  # Split D_star into D_a and D_r
  D_a <- D_star[D_star[,1] > teta, ]
  D_r <- D_star[D_star[,1] <= teta, ]
  
  for (j in 1:j_max) {
    require(Metrics)
    # Train model f_a on D_a
    f_a <- glm(Y ~ ., data = D_a, family = binomial(link = "probit"))
    
    # Train model f_o on concatenated dataset D_a and D_r
    f_o <- glm(Y ~ ., data = rbind(D_a, D_r), family = binomial(link = "probit"))
    
    # Generate dataset D_j
    # b is the bad rate
    D_j <- generate(mu_g, mu_b, sigma_g, sigma_b, b, n_train)
    
    # Predict on D_j using model f_a
    D_j$Y_pred <- predict(f_a, newdata = D_j, type = "response")
    
    # Calculate new threshold teta
    teta <- quantile(D_j$Y_pred, 1 - alpha)
    
    # Split D_j into D_j_a and D_j_r
    D_j_a <- D_j[D_j$Y_pred > teta, ]
    D_j_r <- D_j[D_j$Y_pred <= teta, ]
    
    # Update D_a and D_r
    D_a <- rbind(D_a, D_j_a[, !names(D_j_a) %in% c("Y_pred")])
    D_r <- rbind(D_r, D_j_r[, !names(D_j_a) %in% c("Y_pred")])
    
    print(unique(D_a$Y))
    print(nrow(D_a))
    
    D_r_gal <- D_r
    D_r_gal$Y <- NA
    D_gal <- rbind(D_a, D_r_gal)
    
    #auc_gal <- galimard_option_2(D_gal,'Y',5, D)
    
    # Predict on dataset D using models f_a and f_o
    D$Y_pred <- predict(f_a, newdata = D, type = "response")
    D$Y_pred_fo <- predict(f_o, newdata = D, type = "response")
    
    
    #galimard_option_2(D_gal,'Y',5, D)
    # Print accuracy scores for f_a and f_o every 50 iterations
    if (j %% 50 == 0) {
      print(paste("f_a on run", j, ":", auc(D$Y,D$Y_pred), "\n"))
      print(paste("f_o on run", j, ":", auc(D$Y,D$Y_pred_fo), "\n"))
      
      auc_score_gal = tryCatch(galimard_option_2(D_gal,'Y',5, D), error = function(e) NA)
      print(paste("gal on run", j, ":", print(auc_score_gal), "\n"))
      
      auc_score_ppma = PPMA_MI_sim(D_gal, 'Y', 5, D, 0.5)
      print(paste("ppma on run", j, ":", print(auc_score_ppma), "\n"))
      auc_score_aug <- tryCatch(augment(D_a[, -ncol(D_a)], D_r[, -ncol(D_r)], as.integer(D_a[,ncol(D_a)]),D[, c("V1","V2","V4","V5", "Y")]), error = function(e) NA)
      print(paste("aug on run", j, ":", print(auc_score_aug), "\n"))
      sim_df <- rbind(sim_df, data.frame(combination = i, iteration = j, auc_a = auc(D$Y,D$Y_pred), 
                                         auc_o = auc(D$Y,D$Y_pred_fo), auc_gal = auc_score_gal,
                                         auc_ppma = auc_score_ppma, auc_aug = auc_score_aug))
      print(sim_df)
    }
    filename = paste("results_from_comb", i,".csv")
    write.csv(sim_df, filename)
    
  }
  #print(teta)
  
  # return(list(D_a = D_a, D_r = D_r))
  return(sim_df)
} 

combinations <- read.csv("./combinations_4.csv")


#Latest used data
cov_g = 0.2
cov_b = -0.2

mu_g_list = list(c(1.2,0.6), c(1.5, 0.8), c(1,0.5))
mu_b_list = list(c(2,1))
sigma_g_list = list(c(1, cov_g, cov_g, 1))
sigma_b_list = list(c(1, cov_b, cov_b, 1),c(1, cov_g, cov_g, 1))
m = c(10)
n_train = c (300)
n_test = c (30000)
miss_g = c(0.6)
miss_b = c(0.9)
phi = c(0.5)
j = c (300)
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
  sigma_g <- matrix(current_combination$sigma_g_list[[1]], nrow = 2)
  sigma_b <- matrix(current_combination$sigma_b_list[[1]], nrow = 2)
  b <- current_combination$b
  j <- current_combination$j
  n_train <- current_combination$n_train
  n_test <- current_combination$n_test
  alpha <- current_combination$alpha
  
  df_results[[i]] = simulation_gal(mu_g, mu_b, sigma_g, sigma_b, n_train, n_test, b, alpha, j, i)
}

generate <- function(mu_g, mu_b, sigma_g, sigma_b, b, n) {
  
  # Function to draw weighted sum of samples from two Gaussian distributions
  draw_weighted_sum <- function(mu1, cov1, weight, n_samples=1) {
    # Draw samples from the first normal distribution
    samples1 <- MASS::mvrnorm(n_samples, mu1, cov1)
    
    # Draw samples from the second normal distribution
    mu2 <- mu1 + 5
    samples2 <- MASS::mvrnorm(n_samples, mu2, cov1)
    
    # Calculate the weighted sum of the samples
    weighted_sum_samples <- weight * samples1 + (1 - weight) * samples2
    
    return(weighted_sum_samples)
  }
  
  # Calculate number of samples for each Gaussian distribution
  n_g <- round(n * (1 - b))
  n_b <- n - n_g
  
  # Generate samples for Gaussian distributions
  x_g <- as.data.frame(draw_weighted_sum(mu_g, sigma_g, 0.7, n_g))
  x_g_3 <- rnorm(n_g, mean = 0, sd = 1)
  x_g_4 <- rnorm(n_g, mean = 0, sd = 1)
  
  x_b <- as.data.frame(draw_weighted_sum(mu_b, sigma_b, 0.7, n_b))
  x_b_3 <- rnorm(n_b, mean = 0, sd = 1)
  x_b_4 <- rnorm(n_b, mean = 0, sd = 1)
  
  # Set class labels
  y_g <- rep(0, n_g)
  y_b <- rep(1, n_b)
  
  # Combine features and class labels
  
  final_g <- cbind(x_g, V4 = x_g_3, Y = y_g)
  final_b <- cbind(x_b, V4 = x_b_3, Y = y_b)
  
  final_g <- cbind(x_g, V4 = x_g_3, V5 = x_g_4, Y = y_g)
  final_b <- cbind(x_b, V4 = x_b_3, V5 = x_b_4, Y = y_b)
  
  # Combine data from both Gaussian distributions
  data <- rbind(final_g, final_b)
  
  return(data)
}

galimard_option_2 <- function (data, varMNAR, m, test_data){
  
  require(miceMNAR)
  require(mice)
  
  JointModelEq <- generate_JointModelEq(data=data,varMNAR = varMNAR)
  JointModelEq[,"Y_var_sel"] <- c(1,1,1,1,1,0)
  JointModelEq[,"Y_var_out"] <- c(1,1,1,1,0,0)
  arg <- MNARargument(data=data,varMNAR=varMNAR,JointModelEq=JointModelEq)
  imp <- mice(data = arg$data_mod,
              method = arg$method,
              predictorMatrix = arg$predictorMatrix,
              JointModelEq=arg$JointModelEq,
              control=arg$control, maxit=2, m = m , print = FALSE)
  fit <- with(imp, glm(Y ~ V1 + V2 + V3 + V4 + V5, family=binomial(link = "probit")))
  
  
  
  # trying to pool the results after getting them for each of the models 
  # fitted to each imputed dataset
  predm <- lapply(getfit(fit), predict, newdata= test_data [, c("V1", "V2", "V3", "V4", "V5")], se.fit = TRUE, type = "response")
  
  Q <- sapply(predm, `[[`, "fit")
  U <- sapply(predm, `[[`, "se.fit")^2
  dfcom <- predm[[1]]$df
  dfcom <- 0
  
  # pool predictions
  pred <- matrix(NA, nrow = nrow(Q), ncol = 3,
                 dimnames = list(NULL, c("fit", "se.fit", "df")))
  for(i in 1:nrow(Q)) {
    pi <- pool.scalar(Q[i, ], U[i, ], n = dfcom + 1)
    pred[i, 1] <- pi[["qbar"]]
    pred[i, 2] <- sqrt(pi[["t"]])
    pred[i, 3] <- pi[["df"]]
  }
  
  #Creating predictions pooled based on Rubin's rules
  pred = as.data.frame(pred)
  require(pROC)
  require(Metrics)
  require(PRROC)
  
  # Compute ROC curve
  #roc_curve <- roc(response = test_data$Y, predictor = pred$fit)
  # Calculate AUC
  #auc_value <- auc(roc_curve)
  auc_value = auc(test_data[, c("Y")], pred$fit)
  # Print AUC
  auc_pr = pr.curve(scores.class0 = test_data[, c("Y")] , weights.class0 = pred$fit)$auc.integral
  #print(paste("AUC_ROC from Gal:", auc_value))
  #print(paste("AUC_PR from Gal:", auc_pr))
  
  return ((auc_value) 
          #pred$fit
          #auc_pr
  )
  
}

# PPMI - adjusting to match the structure of the Lessmann acceptance loop

PPMA_MI_sim <- function(data, varMNAR, m, test_data, phi) {
  
  source("./src/binaryPPMA_functions.R")
   
  #Getting imputed values from paper function
  mult0 <- mi(data[, which(names(data) == varMNAR)], data[, -which(names(data) == varMNAR)], phi=phi, drawphi=FALSE, D=m, burnin=20, thin=100)  # phi=0
  
  for (i in 0:ncol(mult0)) {
    if (i == 0){
      target_index <- which(names(data) == varMNAR)
      modified_data_0 = data[, c(setdiff(1:ncol(data), target_index), target_index)]
      #modified_data_0 <- dat[,c(2,3,1)]
      modified_data_0$i = i
      colnames(modified_data_0) = c("V1","V2","V3","V4","V5","Y", ".imp")
    } else{
      modified_data <- cbind(data[, -which(names(data) == varMNAR)], mult0[,i])
      modified_data$i = rep(i, 1, nrow(modified_data))
      colnames(modified_data) = c("V1", "V2","V3","V4","V5","Y", ".imp")
      modified_data_0 = rbind(modified_data_0, modified_data)
    }
    # names(modified_data)[names(modified_data) == 'i'] <- '.imp'
    
    #dataset_list[[i]] = as.mids(modified_data)
  }
  modified_data_0 = as.mids(modified_data_0)
  
  fit <- with(modified_data_0, glm(Y ~ V1+V2+V3+V4+V5, family=binomial))
  
  # trying to pool the results after getting them for each of the models 
  # fitted to each imputed dataset
  predm <- lapply(getfit(fit), predict, newdata=test_data[,c ("V1", "V2", "V3", "V4", "V5")], se.fit = TRUE, type = "response")
  
  Q <- sapply(predm, `[[`, "fit")
  U <- sapply(predm, `[[`, "se.fit")^2
  dfcom <- predm[[1]]$df
  dfcom <- 0
  
  # pool predictions
  pred <- matrix(NA, nrow = nrow(Q), ncol = 3,
                 dimnames = list(NULL, c("fit", "se.fit", "df")))
  for(i in 1:nrow(Q)) {
    pi <- pool.scalar(Q[i, ], U[i, ], n = dfcom + 1)
    pred[i, 1] <- pi[["qbar"]]
    pred[i, 2] <- sqrt(pi[["t"]])
    pred[i, 3] <- pi[["df"]]
  }
  
  #Creating predictions pooled based on Rubin's rules
  pred = as.data.frame(pred)
  require(pROC)
  require(Metrics)
  require(PRROC)
  
  # Compute ROC curve
  #roc_curve <- roc(response = test_data[, c("Y")], predictor = pred$fit)
  # Calculate AUC
  #auc_value <- auc(roc_curve, pred$fit)
  
  # Print AUC
  #print(auc_value)
  auc_value = auc(test_data[, c("Y")], pred$fit)
  print(paste("AUC_ROC from PPMA:", auc_value))
  auc_pr = pr.curve(scores.class0 = test_data[, c("Y")] , weights.class0 = pred$fit)$auc.integral
  #print(paste("AUC_PR from PPMA:", auc_pr))
  
  return (auc_value 
    #pred$fit
    #auc_pr
    )
}

augment <- function(xf, xnf, yf, test_data){
  model <- augmentation(xf, xnf, yf)
  pred_aug <- predict(model,newdata= test_data[, -ncol(test_data)], type = "response")
  return (auc(test_data$Y,pred_aug))
}

# Function to create missing values
missingness_mnar <- function(data, missing_percentage_0, missing_percentage_1) {
  data$Y_hat <- data$Y
  
  sampled_indices_0 <- sample(which(data$Y == 0), size = floor(length(which(data$Y == 0)) * missing_percentage_0))
  data$Y_hat[sampled_indices_0] <- NA
  
  sampled_indices_1 <- sample(which(data$Y == 1), size = floor(length(which(data$Y == 1)) * missing_percentage_1))
  data$Y_hat[sampled_indices_1] <- NA
  
  data$r <- ifelse(is.na(data$Y_hat), 1, 0)
  
  return(data)
}

# Data Generation from a single Gaussian distribution
generate_single <- function(mu_g, mu_b, sigma_g, sigma_b, b, n) {
  n_b <- as.integer(b * n)
  n_g <- as.integer(n - b * n)
  
  x_g <- matrix(rmvnorm(n_g, mean = mu_g, sigma = sigma_g), ncol = length(mu_g), byrow = TRUE)
  x_b <- matrix(rmvnorm(n_b, mean = mu_b, sigma = sigma_b), ncol = length(mu_b), byrow = TRUE)
  
  y_g <- 0
  y_b <- 1
  
  final_g <- as.data.frame(x_g)
  colnames(final_g) <- paste0("V", 1:length(mu_g))
  final_g$Y <- y_g
  
  final_b <- as.data.frame(x_b)
  colnames(final_b) <- paste0("V", 1:length(mu_b))
  final_b$Y <- y_b
  
  data <- rbind(final_g, final_b)
  return(data)
}


simulation_gal <- function(mu_g, mu_b, sigma_g, sigma_b, n_train, n_test, b, alpha, j_max, i) {
  # Generate initial dataset D_star
  
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
    
    #print(unique(D_a$Y))
    #print(nrow(D_a))
    
    D_r_gal <- D_r
    D_r_gal$Y <- NA
    D_gal <- rbind(D_a, D_r_gal)
    
    #auc_gal <- galimard_option_2(D_gal,'Y',5, D)
    
    # Predict on dataset D using models f_a and f_o
    D$Y_pred <- predict(f_a, newdata = D, type = "response")
    D$Y_pred_fo <- predict(f_o, newdata = D, type = "response")
    
    
    #galimard_option_2(D_gal,'Y',5, D)
    # Print accuracy scores for f_a and f_o every 50 iterations
    if (j %% 10 == 0) {
      print(paste("f_a on run", j, ":", auc(D$Y,D$Y_pred), "\n"))
      print(paste("f_o on run", j, ":", auc(D$Y,D$Y_pred_fo), "\n"))
      
      # Results from Galimard
      auc_score_gal = tryCatch(galimard_option_2(D_gal,'Y',5, D), error = function(e) NA)
      print(paste("gal on run", j, ":", print(auc_score_gal), "\n"))
      
      #Results from PPMA
      auc_score_ppma = PPMA_MI_sim(D_gal, 'Y', 5, D, 0.5)
      print(paste("ppma on run", j, ":", print(auc_score_ppma), "\n"))
      
      #Results from Augmentation
      auc_score_aug <- tryCatch(augment(D_a[, -ncol(D_a)], D_r[, -ncol(D_r)], as.integer(D_a[,ncol(D_a)]),D[, c("V1","V2","V3","V4","V5", "Y")]), error = function(e) NA)
      print(paste("aug on run", j, ":", print(auc_score_aug), "\n"))
      
      #Storing results
      sim_df <- rbind(sim_df, data.frame(combination = i, iteration = j, auc_a = auc(D$Y,D$Y_pred), 
                                         auc_o = auc(D$Y,D$Y_pred_fo), auc_gal = auc_score_gal,
                                         auc_ppma = auc_score_ppma, auc_aug = auc_score_aug))
      
      #Showing the output
      print(sim_df)
    }
    
    #storing results
    filename = paste("results_from_comb", i,".csv")
    write.csv(sim_df, filename)
    
  }
  #print(teta)
  
  # return(list(D_a = D_a, D_r = D_r))
  return(sim_df)
} 



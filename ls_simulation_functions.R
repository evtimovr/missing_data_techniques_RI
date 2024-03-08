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
  
  setwd("/Users/radoslavevtimov/Desktop/Master Thesis/PPMA-master")
  source("./binaryPPMA_functions.R")
   
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

augment(D_a[, -ncol(D_a)], D_r[, -ncol(D_r)],D_a$Y, D)
model <- augmentation(D_a[, -ncol(D_a)], D_r[, -ncol(D_r)], as.factor(D_a$Y))

df <- generate_data(n = 100, d = 2)
xf <- df[, -ncol(df)]
yf <- df$y
# We simulate data from not financed clients (MCAR mechanism)
xnf <- generate_data(n = 100, d = 2)[, -ncol(df)]

test_data -> 

augment()

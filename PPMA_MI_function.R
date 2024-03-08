install.packages("Metrics")
library(Metrics)

########################
# Multiple Imputation
########################
## Perform imputation (50 imputations)
set.seed(217531)


# Rado: trying to convert the output of MI to march the mids format to be able to produce seperate glms on it

# adjust function to adjust also the phi value, the burnin and the thin 

PPMA_MI <- function(data, varMNAR, m, test_data, phi) {
  
setwd("/Users/radoslavevtimov/Desktop/Master Thesis/PPMA-master")
source("./binaryPPMA_functions.R")

#Getting imputed values from paper function
mult0 <- mi(data[, which(names(data) == varMNAR)], data[, -which(names(data) == varMNAR)], phi=phi,   drawphi=FALSE, D=m, burnin=20, thin=100)  # phi=0

for (i in 0:ncol(mult0)) {
  if (i == 0){
    target_index <- which(names(data) == varMNAR)
    modified_data_0 = data[, c(setdiff(1:ncol(data), target_index), target_index)]
    #modified_data_0 <- dat[,c(2,3,1)]
    modified_data_0$i = i
    colnames(modified_data_0) = c("V1", "V2", "Y_hat", ".imp")
  } else{
    modified_data <- cbind(data[, -which(names(data) == varMNAR)], mult0[,i])
    modified_data$i = rep(i, 1, nrow(modified_data))
    colnames(modified_data) = c("V1", "V2", "Y_hat", ".imp")
    modified_data_0 = rbind(modified_data_0, modified_data)
  }
  # names(modified_data)[names(modified_data) == 'i'] <- '.imp'
  
  #dataset_list[[i]] = as.mids(modified_data)
}
modified_data_0 = as.mids(modified_data_0)

fit <- with(modified_data_0, glm(Y_hat ~ V1 + V2, family=binomial))

# trying to pool the results after getting them for each of the models 
# fitted to each imputed dataset
predm <- lapply(getfit(fit), predict, newdata=test_data[,c ("V1", "V2")], se.fit = TRUE, type = "response")

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
print(paste("AUC_PR from PPMA:", auc_pr))

return (#auc_value, 
        #pred$fit
        auc_pr)
}



#checking how it works
bla = PPMA_MI(dat, "y",50, dat)
train_data_ppma  = generated_data[-c(3,4,6)]
test_data_gal_ppma = test_data[-c(3,4,6)]
bla = PPMA_MI(train_data_ppma, 'Y_hat', 50, test_data_gal_ppma)



#Function with three variables

PPMA_MI_3_var <- function(data, varMNAR, m, test_data) {
  PPMA_MI(train_data_ppma_3_vars, 'Y_hat', current_combination$m, test_data_ppma)
  data = train_data_ppma_3_vars
  varMNAR = 'Y_hat'
  m = current_combination$m
  test_data = test_data_ppma
  #data = dat
  #varMNAR = "y"
  #m = 50
  #test_data = dat
  
  setwd("/Users/radoslavevtimov/Desktop/Master Thesis/PPMA-master")
  source("./binaryPPMA_functions.R")
  
  #Getting imputed values from paper function
  mult0 <- mi(data[, which(names(data) == varMNAR)], data[, -which(names(data) == varMNAR)], phi=0.5,   drawphi=FALSE, D=m, burnin=20, thin=100)  # phi=0
  
  for (i in 0:ncol(mult0)) {
    if (i == 0){
      target_index <- which(names(data) == varMNAR)
      modified_data_0 = data[, c(setdiff(1:ncol(data), target_index), target_index)]
      #modified_data_0 <- dat[,c(2,3,1)]
      modified_data_0$i = i
      colnames(modified_data_0) = c("V1", "V2", "V3", "Y_hat", ".imp")
    } else{
      modified_data <- cbind(data[, -which(names(data) == varMNAR)], mult0[,i])
      modified_data$i = rep(i, 1, nrow(modified_data))
      colnames(modified_data) = c("V1", "V2", "V3", "Y_hat", ".imp")
      modified_data_0 = rbind(modified_data_0, modified_data)
    }
    # names(modified_data)[names(modified_data) == 'i'] <- '.imp'
    
    #dataset_list[[i]] = as.mids(modified_data)
  }
  
  sum(!complete.cases(modified_data_0))
  modified_data_0 = as.mids(modified_data_0)
  sum(!complete.cases(mult0))
  
  fit <- with(modified_data_0, glm(Y_hat ~ V1 + V2 + V3, family=binomial))
  
  # trying to pool the results after getting them for each of the models 
  # fitted to each imputed dataset
  predm <- lapply(getfit(fit), predict, newdata=test_data[,c ("V1", "V2", "V3")], se.fit = TRUE, type = "response")
  
  Q <- sapply(predm, `[[`, "fit")
  U <- sapply(predm, `[[`, "se.fit")^2
  dfcom <- predm[[1]]$df
  dfcom <- 5
  
  # pool predictions
  pred <- matrix(NA, nrow = nrow(Q), ncol = 3,
                 dimnames = list(NULL, c("fit", "se.fit", "df")))
  for(i in 1:nrow(Q)) {
    pi <- pool.scalar(Q[i, ], U[i, ]#, n = dfcom + 1
                      ) # probably issues here
    pred[i, 1] <- pi[["qbar"]]
    pred[i, 2] <- sqrt(pi[["t"]])
    pred[i, 3] <- pi[["df"]]
  }
  
  #Creating predictions pooled based on Rubin's rules
  pred = as.data.frame(pred)
  require(pROC)
  
  # Compute ROC curve
  roc_curve <- roc(response = test_data[, c("Y")], predictor = pred$fit)
  # Calculate AUC
  auc_value <- auc(roc_curve)
  
  # Print AUC
  print(auc_value)
  
  return (auc_value)
}


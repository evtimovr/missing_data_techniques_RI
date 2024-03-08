data_cont_simu <- function(n, d, k) {
  set.seed(k)
  x <- matrix(runif(n * d), nrow = n, ncol = d)
  theta <- c(1, -1)
  log_odd <- x %*% theta
  
  y <- rbinom(n, 1, 1 / (1 + exp(-log_odd)))
  
  return(list(x, y))
}

if (require(ggplot2, quietly = TRUE)) {
  data <- data_cont_simu(100, 2, 1)
  x <- data[[1]]
  y <- data[[2]]
  df <- data.frame(x = x, y = y)
  ggplot(df, aes(x = x.1, y = x.2, colour = factor(y))) +
    geom_point()
  
  data <- data_cont_simu(1000, 2, 1)
  x <- data[[1]]
  y <- data[[2]]
  df <- data.frame(x = x, y = y)
  hat_theta <- glm(y ~ . - 1, data = df, family = binomial(link = "logit"))
  df$decision <- factor(ifelse(predict(hat_theta, df, type = "response") > 0.7, "reject", "accept"))
  ggplot(df, aes(x = x.1, y = x.2, colour = decision)) +
    geom_point()
  
  theta_1 <- matrix(NA, ncol = 1, nrow = 1000)
  theta_2 <- matrix(NA, ncol = 1, nrow = 1000)
  theta_1_f <- matrix(NA, ncol = 1, nrow = 1000)
  theta_2_f <- matrix(NA, ncol = 1, nrow = 1000)
  for (k in 1:1000) {
    data <- data_cont_simu(1000, 2, k)
    x <- data[[1]]
    y <- data[[2]]
    df <- data.frame(x = x, y = y)
    hat_theta <- glm(y ~ . - 1, data = df, family = binomial(link = "logit"))
    
    theta_1[k] <- hat_theta$coefficients[1]
    theta_2[k] <- hat_theta$coefficients[2]
    
    df$decision <- factor(ifelse(predict(hat_theta, df, type = "response") > 0.6, "reject", "accept"))
    hat_theta_f <- glm(y ~ . - 1, data = df[df$decision == "accept", -ncol(df)], family = binomial(link = "logit"))
    
    theta_1_f[k] <- hat_theta_f$coefficients[1]
    theta_2_f[k] <- hat_theta_f$coefficients[2]
  }
  ggplot(data.frame(theta_1), aes(x = theta_1)) +
    geom_histogram() +
    geom_vline(xintercept = 1)
}


install.packages("scoringTools")
library(scoringTools)

xf <- as.matrix(df[df$decision == "accept", c("x.1", "x.2")])
xnf <- as.matrix(df[df$decision == "reject", c("x.1", "x.2")])
yf <- df[df$decision == "accept", "y"]

# function need the xs of financed, xs of non-financed and y(financed)
hat_theta_fuzzy <- fuzzy_augmentation(xf, xnf, yf)

predicted_probs_fuzzy <- predict (hat_theta_fuzzy@infered_model, data = x, type = "response")

auc <- auc(roc(y, predicted_probs_fuzzy))



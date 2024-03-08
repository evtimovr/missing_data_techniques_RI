library(ggplot2)
library(dplyr)
library(readr)

combinations <- combinations %>% mutate(combination = row_number())
combinations_df <- combinations_df %>% mutate(combination = row_number())



#Results from 2nd loop
list_of_files <- list.files(path = "/Users/radoslavevtimov/Desktop/Master Thesis/Results/Results_2",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)

results_combined <- readr::read_csv(list_of_files, id = "file_name")
results_combined <- merge(results_combined, combinations, by = "combination", left.x = TRUE)
results_combined <- merge(results_combined, combinations_df, by = "combination", left.x = TRUE) # copy of previous one bcs of format


results_combined$combination = as.factor(results_combined$combination)
results_combined$alpha = as.factor(results_combined$alpha)
results_combined$phi = as.factor(results_combined$phi)


ggplot(data = results_combined, aes(x = iteration, y = auc_ppma, group = combination, color = combination, linetype = phi)) +
  geom_line() +
  labs(x = "Time", y = "Value", title = "Line Graph Example") +
  scale_color_discrete(name = "Group") +
  scale_linetype_discrete(name = "alpha")

write.csv(results_combined, "results_combined_1.csv")
write.csv(results_3_combined, "results_combined_3.csv")


# Results from loop 3
list_of_files <- list.files(path = "/Users/radoslavevtimov/Desktop/Master Thesis/Results/Results_3",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)

#creating a combination id column in combinations df
combinations_df_3 <- combinations_df_3 %>% mutate(combination = row_number())

results_3_combined <- readr::read_csv(list_of_files, id = "file_name")
#results_combined <- merge(results_combined, combinations, by = "combination", left.x = TRUE)
results_3_combined <- merge(results_3_combined, combinations_df_3, by = "combination", left.x = TRUE) # copy of previous one bcs of format


results_3_combined$combination = as.factor(results_3_combined$combination)
results_3_combined$alpha = as.factor(results_3_combined$alpha)
results_3_combined$phi = as.factor(results_3_combined$phi)

#saving file for results from loop 3
write.csv(results_3_combined, "results_combined_3.csv")
#saving = results_combined
require(tidyr)

# Unpivot or melt the dataset with one column called auc to display the results and the name of the model as category
results_3_long <- pivot_longer(results_3_combined, 
                        cols = starts_with("auc"), 
                        names_to = c(".value", "model"), 
                        names_sep = "_")

results_2_long <- pivot_longer(results_combined, 
                               cols = starts_with("auc"), 
                               names_to = c(".value", "model"), 
                               names_sep = "_")


# Print the long dataset
print(df_long)


ggplot(data = subset(results_3_long, combination == 8 & model %in% c("a", "ppma", "gal")), aes(x = iteration, y = auc, group = model, color = model, linetype = model)) +
  geom_line() +
  labs(x = "Time", y = "Value", title = "Line Graph Example") +
  scale_color_discrete(name = "Model") +
  scale_linetype_discrete(name = "alpha")

combinations_df_3[combinations_df_3$combination == 8,]

#Renaming models for graphs

results_3_long$model[results_3_long$model == "a"] = "accepts"
results_3_long$model[results_3_long$model == "o"] = "oracle"
results_3_long$model[results_3_long$model == "aug"] = "augmentation"
results_3_long$model[results_3_long$model == "gal"] = "miceMNAR"
results_3_long$model[results_3_long$model == "ppma"] = "PPMM"

#Renaming models for graphs

results_2_long$model[results_2_long$model == "a"] = "accepts"
results_2_long$model[results_2_long$model == "o"] = "oracle"
results_2_long$model[results_2_long$model == "aug"] = "augmentation"
results_2_long$model[results_2_long$model == "gal"] = "miceMNAR"
results_2_long$model[results_2_long$model == "ppma"] = "PPMM"

#Figure 1

# Graph template for a specific combination
ggplot(data = subset(results_3_long, combination == 8 ), aes(x = iteration, y = auc, group = model, color = model)) + #, linetype = model)) +
  geom_line() +
  labs(x = "Iteration", y = "AUC-ROC", title = "Model performance over 300 iterations") +
  scale_color_discrete(name = "Model") 
#+ scale_linetype_discrete(name = "Model")

# Graph template for a specific combination
ggplot(data = subset(results_3_long, combination == 8 & model %in% c("miceMNAR", "PPMM", "accepts")), aes(x = iteration, y = auc, group = model, color = model)) + #, linetype = model)) +
  geom_line() +
  labs(x = "Iteration", y = "AUC-ROC", title = "Model performance over 300 iterations") +
  scale_color_discrete(name = "Model") 
#+ scale_linetype_discrete(name = "Model")




# Graph template for a specific variables (Alpha)
ggplot(data = subset(results_3_long, combination %in% c(8,2)), aes(x = iteration, y = auc, group = interaction(model, alpha), color = model, linetype = alpha)) +
  geom_line() +
  labs(x = "Iteration", y = "AUC-ROC", title = "Model performance over 300 iterations") +
  scale_color_discrete(name = "Model") 
#+ scale_linetype_discrete(name = "Model")


# Graph template for a specific variables (Phi)
ggplot(data = subset(results_2_long, combination %in% c(5,7) & model %in% c("PPMM")), aes(x = iteration, y = auc, group = interaction(model, phi), color = model, linetype = phi)) +
  geom_line() +
  labs(x = "Iteration", y = "AUC-ROC", title = "PPMM over 300 iterations") +
  scale_color_discrete(name = "Model") +
theme(plot.title = element_text(hjust = 0.5))

#+ scale_linetype_discrete(name = "Model")

# Graph template for a specific variables (Covariance matrix)
ggplot(data = subset(results_3_long, combination %in% c(2,5)), aes(x = iteration, y = auc, group = interaction(model, sigma_b_list), color = model, linetype = sigma_b_list)) +
  geom_line() +
  labs(x = "Iteration", y = "AUC-ROC", title = "Model performance over 300 iterations") +
  scale_color_discrete(name = "Model") 
#+ scale_linetype_discrete(name = "Model")



# Graph template for a specific variables (Covariance matrix)
ggplot(data = subset(results_3_long, combination %in% c(2,5)), aes(x = iteration, y = auc, group = interaction(model, sigma_b_list), color = model, linetype = sigma_b_list)) +
  geom_line() +
  labs(x = "Iteration", y = "AUC-ROC", title = "Change in covariance over 300 iterations") +
  scale_color_discrete(name = "Model") + 
  scale_linetype_discrete(name = "Covariance matrix (BADs)")+ 
  theme(plot.title = element_text(hjust = 0.5))



#Visualisations for 2-variable implementation 


list_of_files <- list.files(path = "/Users/radoslavevtimov/Desktop/Master Thesis/Results/Results_5_2_var",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)

combinations_df_4 <- read.csv("/Users/radoslavevtimov/Desktop/Master Thesis/Results/combinations_4.csv") 

#creating a combination id column in combinations df
combinations_df_4 <- combinations_df_4 %>% mutate(combination = row_number())

results_4_combined <- readr::read_csv(list_of_files, id = "file_name")
#results_combined <- merge(results_combined, combinations, by = "combination", left.x = TRUE)
results_4_combined <- merge(results_4_combined, combinations_df_4, by = "combination", left.x = TRUE) # copy of previous one bcs of format


results_4_combined$combination = as.factor(results_4_combined$combination)
results_4_combined$alpha = as.factor(results_4_combined$alpha)
results_4_combined$phi = as.factor(results_4_combined$phi)

#saving file for results from loop 3
write.csv(results_3_combined, "results_combined_3.csv")
#saving = results_combined
require(tidyr)


#Remaing models for graphs

results_4_long$model[results_4_long$model == "a"] = "accepts"
results_4_long$model[results_4_long$model == "o"] = "oracle"
results_4_long$model[results_4_long$model == "aug"] = "augmentation"
results_4_long$model[results_4_long$model == "gal"] = "miceMNAR"
results_4_long$model[results_4_long$model == "ppma"] = "PPMM"

# Unpivot or melt the dataset with one column called auc to display the results and the name of the model as category
results_4_long <- pivot_longer(results_4_combined, 
                               cols = starts_with("auc"), 
                               names_to = c(".value", "model"), 
                               names_sep = "_")


# Graph template for a specific variables (Covariance matrix)
ggplot(data = subset(results_4_long, combination %in% c(1)), aes(x = iteration, y = auc, group = interaction(model), color = model)) + #, linetype = combination)) +
  geom_line() +
  labs(x = "Iteration", y = "AUC-ROC", title = "Performance over 300 iterations") +
  scale_color_discrete(name = "Model") + 
  #scale_linetype_discrete(name = "Covariance matrix (BADs)")+ 
  theme(plot.title = element_text(hjust = 0.5))


# Graph template for a specific variables (Covariance matrix)
ggplot(data = subset(results_4_long, combination %in% c(6)), aes(x = iteration, y = auc, group = interaction(model), color = model)) + #, linetype = combination)) +
  geom_line() +
  labs(x = "Iteration", y = "AUC-ROC", title = "Performance over 300 iterations") +
  scale_color_discrete(name = "Model") + 
  #scale_linetype_discrete(name = "Covariance matrix (BADs)")+ 
  theme(plot.title = element_text(hjust = 0.5))


# Graph template for a specific variables (Covariance matrix)
ggplot(data = subset(results_4_long, combination %in% c(2)), aes(x = iteration, y = auc, group = interaction(model), color = model)) + #, linetype = combination)) +
  geom_line() +
  labs(x = "Iteration", y = "AUC-ROC", title = "Performance over 300 iterations") +
  scale_color_discrete(name = "Model") + 
  #scale_linetype_discrete(name = "Covariance matrix (BADs)")+ 
  theme(plot.title = element_text(hjust = 0.5))



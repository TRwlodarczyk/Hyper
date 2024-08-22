

{
  library(ggplot2)
  library(ggpubr)
  library(dplyr)
  library(data.table)
  library(reshape2)
  library(reshape)
  library("readxl")
  library(ggpubr)
  library(agricolae)
  library(tidyverse)
  library (readr) #to read URL
  library(stringr) # For str_replace_all
  library(psych)
  library(car)
  library(openxlsx)
  
}

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research/1_Experiment/1_Experiment_P/Spreadsheet/Machine Learning")
dt <-read.delim("Project_P_pXRF_HP_Hyper.txt")

dt <- dt %>% 
  filter(Hyperspectral_measured == "Yes" &
           Leaf_Type == "pxrf")



dt <- dt %>%
  mutate(VI553_735 = (X550 - X716) / (X550 + X717))




model <- glm(Zn_PXRF_mean ~ X550 + X716,
             data = dt, 
             family = gaussian())
summary(model)

# Predict values using the model
predicted_values <- predict(model, newdata = dt, type = "response")

# Actual values of Zn
actual_values <- dt$Zn

# Calculate residuals
residuals <- actual_values - predicted_values

# Calculate RMSE
rmse <- sqrt(mean(residuals^2))

# Print the RMSE
print(rmse)

dt$Predicted_Zn <- predict(model, newdata = dt, type = "response")



library(ggplot2)

# Create the scatter plot
ggplot(dt, aes(x = Zn_PXRF_mean, y = Predicted_Zn, group = Status, color = Status)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Zn (pXRF)", y = "Predicted Zn", title = "") +
  theme_minimal()+
  scale_color_gradient(low = "blue", high = "red")  # Optional: Adjust the color gradient


absolute_errors <- abs(dt$Zn_PXRF_mean - dt$Predicted_Zn)

# Calculate the mean absolute error
mae <- mean(absolute_errors)


model_zn <- lm(Zn_PXRF_mean ~ Predicted_Zn, data = dt)

# Summary of the model to get R-squared
summary_model_zn <- summary(model_zn)



# to consider:
corr.test(dt$X550, dt$Zn_PXRF_mean) #0.59
corr.test(dt$X600, dt$Zn_PXRF_mean) #0.53
corr.test(dt$X716, dt$Zn_PXRF_mean) #0.6
corr.test(dt$X1950, dt$Zn_PXRF_mean) #-0.01






###### Classification of H A N C (healthy, Anthocyanins, Necrosis, Chlorosis) in the leaves


setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research/1_Experiment/1_Experiment_P/Spreadsheet/Machine Learning")
dt <-read.delim("Project_P_pXRF_HP_Hyper.txt")

library(caTools)

split_values <- sample.split(dt$Status, SplitRatio = 0.65) 
train_set<- subset(dt, split_values==T)
test_set<- subset(dt, split_values==F)

library(rpart)

# Define the columns to use for the prediction
train_subset <- train_set[, c(20, 149:2198)]  # Status is column 20
test_subset <- test_set[, c(20, 149:2198)]    # Status is column 20
mod_class <- rpart(Status ~ ., data = train_subset)
result_class <- predict(mod_class, test_subset, type = "class")
table(test_subset$Status, result_class)


str(dt)
colnames(dt[149])
colnames(dt[20])


###### Clustering of H A N C (healthy, Anthocyanins, Necrosis, Chlorosis) in the leaves 

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research/1_Experiment/1_Experiment_P/Spreadsheet/Machine Learning")
dt <-read.delim("Project_P_pXRF_HP_Hyper.txt")

dt_k <- dt[, 149:2198]
dt_k <- as.matrix(dt_k)
dt_cluster <- kmeans(dt_k, 4)
clustered_data <- cbind(dt, dt_cluster$cluster)

write.xlsx(clustered_data, "Project_P_clustered_data.xlsx")

dt_cluster <- kmeans(dt_k, 3)
clustered_data <- cbind(dt, dt_cluster$cluster)
write.xlsx(clustered_data, "Project_P_3_clustered_data.xlsx")










###### GLM jackknife cross-validation with ACC function for CV20 




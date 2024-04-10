#GLM


library(tidyverse)
library(viridis)
library(dplyr)

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research/1_Experiment/Analysis")
dt <-read.delim("GLM.txt")


model <- glm(Zn ~ IDX703m1417 + IDX707o2347 + IDX1410o1506 + IDX1397m1539 + PSRI + X709 + X529 + X2420 + HP.FvoFm + GNDVI + X1880 + X534 + X727 + X1404 + X1869 + X2353, 
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
ggplot(dt, aes(x = Zn, y = Predicted_Zn, color = `Fresh_biomass..g.`)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Zn (pXRF)", y = "Predicted Zn", title = "") +
  theme_minimal()+
  scale_color_gradient(low = "blue", high = "red")  # Optional: Adjust the color gradient


absolute_errors <- abs(dt$Zn - dt$Predicted_Zn)

# Calculate the mean absolute error
mae <- mean(absolute_errors)


model_zn <- lm(Zn ~ Predicted_Zn, data = dt)

# Summary of the model to get R-squared
summary_model_zn <- summary(model_zn)











#BEST MODEL!
{
modelbest <- glm(Zn ~ Fresh_biomass..g. + WC + IDX703m1417 + IDX707o2347 + IDX1410o1506 + IDX1397m1539 + PSRI   + X709 + X529 + X2420 + X1880 + HP.FvoFm + GNDVI + X534 + X727 + X1404 +X1869  , 
             data = dt, 
             family = gaussian())

summary(modelbest)


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
ggplot(dt, aes(x = Zn, y = Predicted_Zn, color = `Fresh_biomass..g.`)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Zn (pXRF)", y = "Predicted Zn", title = "") +
  theme_minimal()+
  scale_color_gradient(low = "blue", high = "red")  # Optional: Adjust the color gradient


absolute_errors <- abs(dt$Zn - dt$Predicted_Zn)

# Calculate the mean absolute error
mae <- mean(absolute_errors)


model_zn <- lm(Zn ~ Predicted_Zn, data = dt)

# Summary of the model to get R-squared
summary_model_zn <- summary(model_zn)


}




model <- glm(Zn ~ IDX703m1417 + IDX707o2347 + IDX1410o1506 + IDX1397m1539 + PSRI + Fresh_biomass..g.+ WC+ X709+ X529+ X1872+ X1890 + NDWI +WI,
             data = dt, 
             family = gaussian())
summary(model)


model <- glm(Zn ~ IDX703m1417 + IDX707o2347 + IDX1410o1506 + IDX1397m1539 + PSRI + Fresh_biomass..g.+ WC+ X709+ X529+ X1872+ X1890 + NDWI +WI + Root..mm.,
             data = dt, 
             family = gaussian())
summary(model)


model <- glm(Zn ~ IDX703m1417 + IDX707o2347 + IDX1410o1506 + IDX1397m1539,
             data = dt, 
             family = gaussian())
summary(model)


modelbest2 <- glm(Zn ~ Fresh_biomass..g. + WC + IDX703m1417 + IDX707o2347 + IDX1410o1506 + IDX1397m1539 + PSRI +NDWI + WI +X1872 +X1890 +X2353 + X709 + X529 + X2420 + X1880 + HP.FvoFm + GNDVI + X534 + X727 + X1404 +X1869  , 
                 data = dt, 
                 family = gaussian())

summary(modelbest2)



# Predict values using the model
predicted_values <- predict(modelbest2, newdata = dt, type = "response")

# Actual values of Zn
actual_values <- dt$Zn

# Calculate residuals
residuals <- actual_values - predicted_values

# Calculate RMSE
rmse <- sqrt(mean(residuals^2))

# Print the RMSE
print(rmse)

dt$Predicted_Zn <- predict(modelbest2, newdata = dt, type = "response")



library(ggplot2)

# Create the scatter plot
ggplot(dt, aes(x = Zn, y = Predicted_Zn, color = `Fresh_biomass..g.`)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Zn (pXRF)", y = "Predicted Zn", title = "") +
  theme_minimal()+
  scale_color_gradient(low = "blue", high = "red")  # Optional: Adjust the color gradient


absolute_errors <- abs(dt$Zn - dt$Predicted_Zn)

# Calculate the mean absolute error
mae <- mean(absolute_errors)


model_zn <- lm(Zn ~ Predicted_Zn, data = dt)

# Summary of the model to get R-squared
summary_model_zn <- summary(model_zn)

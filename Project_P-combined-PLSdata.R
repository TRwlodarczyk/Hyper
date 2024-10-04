


#libraries
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


setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research/1_Experiment/1_Experiment_P/Spreadsheet/PLS/cv20combined")
dt <-read.delim("Project_P_PLS-combined.txt")


#dt <- dt %>%
#  filter(Zn_PXRF_cv < 0.2)


##from PLScv20withHandypea
dt <- dt %>%
  mutate(DVI588_592 = (X588 - X592))
dt <- dt %>%
  mutate(DVI524_600 = (X524 - X600))
dt <- dt %>%
  mutate(DVI524_600 = (X524 - X600))
dt <- dt %>%
  mutate(NgammaRC_X553 = (gamma.RC....1.gamma.RC.. - X553)/(gamma.RC....1.gamma.RC.. + X553))
dt <- dt %>%
  mutate(NVI721_722 = (X721 - X722)/(X721+X722))
dt <- dt %>%
  mutate(NVI550_730 = (X550 - X730)/(X550+X730))
dt <- dt %>%
  mutate(RgammaRC_X553 = (gamma.RC....1.gamma.RC..)/(X553))
dt <- dt %>%
  mutate(RVI721_723 = (X721/X723))
dt <- dt %>%
  mutate(RVI550_724 = (X550/X724))
# 719, 550

##from PLScv20no Handy

dt <- dt %>%
  mutate(NVI548_725 = (X548 - X725)/(X548+X725))

dt <- dt %>%
  mutate(RVI721_723 = (X721/X723))
dt <- dt %>%
  mutate(RVI549_724 = (X549/X724))
dt <- dt %>%
  mutate(DVI546_549 = (X546 - X549))
dt <- dt %>%
  mutate(DVI546_711 = (X546 - X711))

#719

##from PLScv20no Healthy

dt <- dt %>%
  mutate(DVI541_711 = (X541 - X711))
dt <- dt %>%
  mutate(RVI550_722 = (X550/X722))
dt <- dt %>%
  mutate(NVI522_724 = (X522 - X724)/(X522+X724))
dt <- dt %>%
  mutate(RVI721_722 = (X721/X722))
dt <- dt %>%
  mutate(DVI542_546 = (X542 - X546))
#717

##from PLScv20no Chlorosis

#2213


##from PLScv20no Anthocyanins
dt <- dt %>%
  mutate(NVI707_708 = (X707 - X708)/(X707+X708))

dt <- dt %>%
  mutate(RVI707_708 = (X707/X708))

dt <- dt %>%
  mutate(DVI581_582 = (X581 - X582))

dt <- dt %>%
  mutate(NVI531_570 = (X531 - X570)/(X531+X570))

dt <- dt %>%
  mutate(RVI531_570 = (X531/X570))
dt <- dt %>%
  mutate(DVI524_575 = (X524 - X575))

#702

write.xlsx(dt, "Project_P_PLS-combined_indicies.xlsx")




# Remove highly correlated





#### Create indicies from indicies
setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research/1_Experiment/1_Experiment_P/Spreadsheet/PLS/cv20combined")
dt <-read.delim("Project_P_PLS-combined_indicies.txt")



{
  
  colnames(dt)[89] #here the hyper cols starts 
  dt[, 89:269] <- lapply(dt[, 89:269], as.numeric)
  
  
  # Get column names for the relevant columns (89 to 236)
  relevant_columns <- names(dt)[89:269]
  
  # Function to calculate the index for a pair of columns
  calculate_index <- function(col1, col2) {
    index <- (dt[[col1]] - dt[[col2]]) / (dt[[col1]] + dt[[col2]])
    # Handle Inf and NaN by replacing them with NA
    index[is.infinite(index) | is.nan(index)] <- NA
    return(index)
  }
  
  # Create a dataframe to hold the names of new indices and their R-squared values
  results <- data.frame(Index = character(), R_squared = numeric(), stringsAsFactors = FALSE)
  
  # Iterate over the pairs of columns to create new indices and calculate R-squared values
  for (i in 1:(length(relevant_columns) - 1)) {
    for (j in (i + 1):length(relevant_columns)) {
      col1 <- relevant_columns[i]
      col2 <- relevant_columns[j]
      new_col_name <- paste(col1, col2, sep = "_")
      
      # Calculate the index
      dt[[new_col_name]] <- calculate_index(col1, col2)
      
      # Skip if all values in the new index column are NA
      if (all(is.na(dt[[new_col_name]]))) next
      
      # Remove rows with NA in the new index column and Zn_PXRF_mean
      valid_rows <- !is.na(dt[[new_col_name]]) & !is.na(dt$Zn_PXRF_mean)
      
      # Fit the linear model and calculate R-squared
      model <- lm(dt[[new_col_name]][valid_rows] ~ dt$Zn_PXRF_mean[valid_rows])
      r_squared <- summary(model)$r.squared
      
      # Add the result to the results dataframe
      results <- rbind(results, data.frame(Index = new_col_name, R_squared = r_squared))
    }
  }
  
  # Display the results
  results
  
  
  write.xlsx(results, "Project_P_PLS-cv20-combined_Normalized_Index.xlsx")
  
  
  #############################these are yet to be analyzed below:
  
  
  
  
  relevant_columns <- names(dt)[89:269]
  
  
  calculate_index <- function(col1, col2) {
    index <- (dt[[col1]]) / (dt[[col2]])
    # Handle Inf and NaN by replacing them with NA
    index[is.infinite(index) | is.nan(index)] <- NA
    return(index)
  }
  
  # Create a dataframe to hold the names of new indices and their R-squared values
  results <- data.frame(Index = character(), R_squared = numeric(), stringsAsFactors = FALSE)
  
  # Iterate over the pairs of columns to create new indices and calculate R-squared values
  for (i in 1:(length(relevant_columns) - 1)) {
    for (j in (i + 1):length(relevant_columns)) {
      col1 <- relevant_columns[i]
      col2 <- relevant_columns[j]
      new_col_name <- paste(col1, col2, sep = "_")
      
      # Calculate the index
      dt[[new_col_name]] <- calculate_index(col1, col2)
      
      # Skip if all values in the new index column are NA
      if (all(is.na(dt[[new_col_name]]))) next
      
      # Remove rows with NA in the new index column and Zn_PXRF_mean
      valid_rows <- !is.na(dt[[new_col_name]]) & !is.na(dt$Zn_PXRF_mean)
      
      # Fit the linear model and calculate R-squared
      model <- lm(dt[[new_col_name]][valid_rows] ~ dt$Zn_PXRF_mean[valid_rows])
      r_squared <- summary(model)$r.squared
      
      # Add the result to the results dataframe
      results <- rbind(results, data.frame(Index = new_col_name, R_squared = r_squared))
    }
  }
  
  # Display the results
  results
  
  write.xlsx(results, "Project_P_PLS-cv20-combined_Ratio_Index.xlsx")
  
  
  
  
  
  relevant_columns <- names(dt)[89:269]
  
  
  calculate_index <- function(col1, col2) {
    index <- (dt[[col1]]) - (dt[[col2]])
    # Handle Inf and NaN by replacing them with NA
    index[is.infinite(index) | is.nan(index)] <- NA
    return(index)
  }
  
  # Create a dataframe to hold the names of new indices and their R-squared values
  results <- data.frame(Index = character(), R_squared = numeric(), stringsAsFactors = FALSE)
  
  # Iterate over the pairs of columns to create new indices and calculate R-squared values
  for (i in 1:(length(relevant_columns) - 1)) {
    for (j in (i + 1):length(relevant_columns)) {
      col1 <- relevant_columns[i]
      col2 <- relevant_columns[j]
      new_col_name <- paste(col1, col2, sep = "_")
      
      # Calculate the index
      dt[[new_col_name]] <- calculate_index(col1, col2)
      
      # Skip if all values in the new index column are NA
      if (all(is.na(dt[[new_col_name]]))) next
      
      # Remove rows with NA in the new index column and Zn_PXRF_mean
      valid_rows <- !is.na(dt[[new_col_name]]) & !is.na(dt$Zn_PXRF_mean)
      
      # Fit the linear model and calculate R-squared
      model <- lm(dt[[new_col_name]][valid_rows] ~ dt$Zn_PXRF_mean[valid_rows])
      r_squared <- summary(model)$r.squared
      
      # Add the result to the results dataframe
      results <- rbind(results, data.frame(Index = new_col_name, R_squared = r_squared))
    }
  }
  
  # Display the results
  results
  
  
  write.xlsx(results, "Project_P_PLS-cv20-combined_Difference_Index.xlsx")
  
  
  
  
}


#Testing best indicies


dt <- dt %>%
  mutate(CB1 = ((X588-X592)*X722)/X550)

corr.test(dt$CB1, dt$Zn_PXRF_mean) # 0.73

dt_C <- subset(dt, Status=="C")
corr.test(dt_C$CB1, dt_C$Zn_PXRF_mean) # 0.36
dt_A <- subset(dt, Status=="A")
corr.test(dt_A$CB1, dt_A$Zn_PXRF_mean) # 0.58
dt_H <- subset(dt, Status=="H")
corr.test(dt_H$CB1, dt_H$Zn_PXRF_mean) # 0.65


ggplot(dt, aes(x = Zn_PXRF_mean , y = CB1, color = Status)) + 
  geom_point(size=3) +  # adds the scatter plot points
  theme_minimal() + 
  labs(title = "",
       x = "Zn conc in leaf",
       y = "CB1")



dt_filtered <- dt %>% filter(CB1 >= 0.003)


ggplot(dt_filtered, aes(x = Zn_PXRF_mean , y = CB1, color = Status)) + 
  geom_point(size=3) +  # adds the scatter plot points
  theme_minimal() + 
  labs(title = "",
       x = "Zn conc in leaf",
       y = "CB1")

corr.test(dt_filtered$CB1, dt_filtered$Zn_PXRF_mean) # 0.65

model <- lm(dt_filtered$CB1~dt_filtered$Zn_PXRF_mean)
summary(model)



dt <- dt %>%
  mutate(CB2 = ((X588-X592)-(X550/X722))/((X588-X592)+(X550/X722)))

ggplot(dt, aes(x = Zn_PXRF_mean , y = CB2, color = Status)) + 
  geom_point(size=3) +  # adds the scatter plot points
  theme_minimal() + 
  labs(title = "",
       x = "Zn conc in leaf",
       y = "CB2")



dt <- dt %>%
  mutate(CB3 = X524 - X600 - X542 - X546) 



ggplot(dt, aes(x = Zn_PXRF_mean , y = CB3, color = Status)) + 
  geom_point(size=3) +  # adds the scatter plot points
  theme_minimal() + 
  labs(title = "",
       x = "Zn conc in leaf",
       y = "CB3")



dt <- dt %>%
  mutate(CB4 = X524 - X600) 

ggplot(dt, aes(x = Zn_PXRF_mean , y = CB4, color = Status)) + 
  geom_point(size=3) +  # adds the scatter plot points
  theme_minimal() + 
  labs(title = "",
       x = "Zn conc in leaf",
       y = "CB3")




####

#### Create even more combinations of indicies
setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research/1_Experiment/1_Experiment_P/Spreadsheet/PLS/cv20combined")
dt <-read.delim("Project_P_PLS-combined_indicies.txt")



dt <- dt %>%
  mutate(CB1 = ((X588-X592)*X722)/X550) #ok

dt <- dt %>%
  mutate(CB2 = ((X588-X592)-(X550/X722))/((X588-X592)+(X550/X722))) #ok

dt <- dt %>%
  mutate(CB3 = X524 - X600 - X542 - X546) # ok

dt <- dt %>%
  mutate(CB4 = X524 - X600) # ok


dt <- dt %>%
  mutate(CB5 = (X718 - DVI588_592)/(X718 + DVI588_592)) #ok


dt <- dt %>%
  mutate(CB6 = (X2213 - DVI588_592)/(X2213 + DVI588_592)) #ok


dt <- dt %>%
  mutate(RVI497_516 = (X497/X516)) #ok


dt <- dt %>%
  mutate(Sm_DVI524_600 = (Sm/DVI524_600)) #ok



ggplot(dt, aes(x = Zn_PXRF_mean , y = Sm_DVI524_600, color = Status)) + 
  geom_point(size=3) +  # adds the scatter plot points
  theme_minimal() + 
  labs(title = "",
       x = "Zn conc in leaf",
       y = "Sm_DVI524_600")


dt_filtered <- dt %>% filter(Sm_DVI524_600 < 5000)
corr.test(dt_filtered$Sm_DVI524_600, dt_filtered$Zn_PXRF_mean) #


write.xlsx(dt, "Project_P_PLS-cv20-combined_indicies_double.xlsx") # likely not useful, PLS weak









colnames(dt)[89] #here the hyper cols starts 
dt[, 89:277] <- lapply(dt[, 89:277], as.numeric)


# Get column names for the relevant columns (89 to 236)
relevant_columns <- names(dt)[89:277]

# Function to calculate the index for a pair of columns
calculate_index <- function(col1, col2) {
  index <- (dt[[col1]] - dt[[col2]]) / (dt[[col1]] + dt[[col2]])
  # Handle Inf and NaN by replacing them with NA
  index[is.infinite(index) | is.nan(index)] <- NA
  return(index)
}

# Create a dataframe to hold the names of new indices and their R-squared values
results <- data.frame(Index = character(), R_squared = numeric(), stringsAsFactors = FALSE)

# Iterate over the pairs of columns to create new indices and calculate R-squared values
for (i in 1:(length(relevant_columns) - 1)) {
  for (j in (i + 1):length(relevant_columns)) {
    col1 <- relevant_columns[i]
    col2 <- relevant_columns[j]
    new_col_name <- paste(col1, col2, sep = "_")
    
    # Calculate the index
    dt[[new_col_name]] <- calculate_index(col1, col2)
    
    # Skip if all values in the new index column are NA
    if (all(is.na(dt[[new_col_name]]))) next
    
    # Remove rows with NA in the new index column and Zn_PXRF_mean
    valid_rows <- !is.na(dt[[new_col_name]]) & !is.na(dt$Zn_PXRF_mean)
    
    # Fit the linear model and calculate R-squared
    model <- lm(dt[[new_col_name]][valid_rows] ~ dt$Zn_PXRF_mean[valid_rows])
    r_squared <- summary(model)$r.squared
    
    # Add the result to the results dataframe
    results <- rbind(results, data.frame(Index = new_col_name, R_squared = r_squared))
  }
}

# Display the results
results


write.xlsx(results, "Project_P_PLS-cv20-combined_Normalized_Index_DOUBLE.xlsx")



dt2 <- dt %>%
  mutate(CB1_CB2 = (CB1-CB2)/(CB1+CB2)) 


ggplot(dt2, aes(x = Zn_PXRF_mean , y = CB1_CB2, color = Status)) + 
  geom_point(size=3) +  # adds the scatter plot points
  theme_minimal() + 
  labs(title = "",
       x = "Zn conc in leaf",
       y = "CB1 - CB2 / CB1 + CB2")








relevant_columns <- names(dt)[89:277]


calculate_index <- function(col1, col2) {
  index <- (dt[[col1]]) / (dt[[col2]])
  # Handle Inf and NaN by replacing them with NA
  index[is.infinite(index) | is.nan(index)] <- NA
  return(index)
}

# Create a dataframe to hold the names of new indices and their R-squared values
results <- data.frame(Index = character(), R_squared = numeric(), stringsAsFactors = FALSE)

# Iterate over the pairs of columns to create new indices and calculate R-squared values
for (i in 1:(length(relevant_columns) - 1)) {
  for (j in (i + 1):length(relevant_columns)) {
    col1 <- relevant_columns[i]
    col2 <- relevant_columns[j]
    new_col_name <- paste(col1, col2, sep = "_")
    
    # Calculate the index
    dt[[new_col_name]] <- calculate_index(col1, col2)
    
    # Skip if all values in the new index column are NA
    if (all(is.na(dt[[new_col_name]]))) next
    
    # Remove rows with NA in the new index column and Zn_PXRF_mean
    valid_rows <- !is.na(dt[[new_col_name]]) & !is.na(dt$Zn_PXRF_mean)
    
    # Fit the linear model and calculate R-squared
    model <- lm(dt[[new_col_name]][valid_rows] ~ dt$Zn_PXRF_mean[valid_rows])
    r_squared <- summary(model)$r.squared
    
    # Add the result to the results dataframe
    results <- rbind(results, data.frame(Index = new_col_name, R_squared = r_squared))
  }
}

# Display the results
results

write.xlsx(results, "Project_P_PLS-cv20-combined_Ratio_Index_DOUBLE.xlsx")



dt2 <- dt %>%
  mutate(CB1_CB6 = (CB1/CB6)) 


ggplot(dt2, aes(x = Zn_PXRF_mean , y = CB1_CB6, color = Status)) + 
  geom_point(size=3) +  # adds the scatter plot points
  theme_minimal() + 
  labs(title = "",
       x = "Zn conc in leaf",
       y = "CB1 - CB2 / CB1 + CB2")

corr.test(dt2$CB1_CB6, dt2$Zn_PXRF_mean) #

dt2 <- dt2 %>% filter(CB1_CB6 >= 0.003)






relevant_columns <- names(dt)[89:277]


calculate_index <- function(col1, col2) {
  index <- (dt[[col1]]) - (dt[[col2]])
  # Handle Inf and NaN by replacing them with NA
  index[is.infinite(index) | is.nan(index)] <- NA
  return(index)
}

# Create a dataframe to hold the names of new indices and their R-squared values
results <- data.frame(Index = character(), R_squared = numeric(), stringsAsFactors = FALSE)

# Iterate over the pairs of columns to create new indices and calculate R-squared values
for (i in 1:(length(relevant_columns) - 1)) {
  for (j in (i + 1):length(relevant_columns)) {
    col1 <- relevant_columns[i]
    col2 <- relevant_columns[j]
    new_col_name <- paste(col1, col2, sep = "_")
    
    # Calculate the index
    dt[[new_col_name]] <- calculate_index(col1, col2)
    
    # Skip if all values in the new index column are NA
    if (all(is.na(dt[[new_col_name]]))) next
    
    # Remove rows with NA in the new index column and Zn_PXRF_mean
    valid_rows <- !is.na(dt[[new_col_name]]) & !is.na(dt$Zn_PXRF_mean)
    
    # Fit the linear model and calculate R-squared
    model <- lm(dt[[new_col_name]][valid_rows] ~ dt$Zn_PXRF_mean[valid_rows])
    r_squared <- summary(model)$r.squared
    
    # Add the result to the results dataframe
    results <- rbind(results, data.frame(Index = new_col_name, R_squared = r_squared))
  }
}

# Display the results
results


write.xlsx(results, "Project_P_PLS-cv20-combined_Difference_Index_DOUBLE.xlsx")



dt3 <- dt %>%
  mutate(CB2_CB6 = (CB2 - CB6)) 


ggplot(dt3, aes(x = Zn_PXRF_mean , y = CB2_CB6, color = Status)) + 
  geom_point(size=3) +  # adds the scatter plot points
  theme_minimal() + 
  labs(title = "",
       x = "Zn conc in leaf",
       y = "CB1 - CB2 / CB1 + CB2")

corr.test(dt3$CB2_CB6, dt3$Zn_PXRF_mean) #0.73
dt3 <- dt3 %>% filter(CB2_CB6 >= -1.9875)





################## NICE PLOT FOR POSTER


#### Create even more combinations of indicies
setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research/1_Experiment/1_Experiment_P/Spreadsheet/PLS/cv20combined")
dt <-read.delim("Project_P_PLS-combined_indicies.txt")



dt <- dt %>%
  mutate(CB1 = ((X588-X592)*X722)/X550) #ok

dt <- dt %>% filter(CB1 >= 0.003)

library(ggplot2)

# Define manual colors: purple, light green, dark green
manual_colors <- c("#6a0dad", "#9acd32", "#006400")  # Purple, Light Green, Dark Green

# Refined scatter plot using the manually defined colors
ggplot(dt, aes(x = Zn_PXRF_mean, y = CB1, color = Status)) + 
  geom_point(size = 4, alpha = 0.8, shape = 21, stroke = 1.3) +  # Adjust point size, transparency, and shape for better visibility
  scale_color_manual(values = manual_colors) +  # Manually set colors
  theme_minimal(base_size = 15) +  # Use minimal theme with slightly larger base font size
  labs(title = "",
       x = "Zn (mg/kg)",  # Clarify the unit in the x-axis label
       y = "Zn-sensitive Vegetation Index") +  # Tweak y-axis label wording
  theme(legend.position = "bottom",  # Place the legend at the bottom for better layout
        legend.title = element_blank(),  # Remove the legend title
        panel.grid.minor = element_blank(),  # Remove minor grid lines for a cleaner look
        panel.grid.major = element_line(color = "grey90"))  # Lighten the grid lines for better contrast


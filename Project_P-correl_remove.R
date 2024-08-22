
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

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research/1_Experiment/1_Experiment_P/Spreadsheet")
dt <-read.delim("Project_P_correlation.txt")


# Remove the row 
dt <- dt %>%
  slice(-2051)

#remove last column
dt <- dt %>% 
  select(-2052)







# Load necessary libraries
library(dplyr)

# Function to find highly correlated wavelengths and retain only one from each group
reduce_correlated_wavelengths <- function(df, threshold = 0.99) {
  # Ensure df only contains numeric columns
  df_numeric <- df %>% select_if(is.numeric)
  
  # Compute the correlation matrix
  cor_matrix <- cor(df_numeric, use = "pairwise.complete.obs")
  
  # Create an empty list to store the wavelengths to keep
  wavelengths_to_keep <- c()
  
  # Create an empty vector to track wavelengths that have been grouped
  grouped_wavelengths <- rep(FALSE, ncol(cor_matrix))
  
  # Loop through each column (wavelength) in the correlation matrix
  for (i in 1:ncol(cor_matrix)) {
    if (!grouped_wavelengths[i]) {
      # Find indices of highly correlated wavelengths
      correlated_indices <- which(abs(cor_matrix[, i]) > threshold & abs(cor_matrix[, i]) < 1)
      
      if (length(correlated_indices) > 0) {
        # Mark these indices as grouped
        grouped_wavelengths[correlated_indices] <- TRUE
        grouped_wavelengths[i] <- TRUE
        # Keep the first wavelength in the group
        wavelengths_to_keep <- c(wavelengths_to_keep, colnames(cor_matrix)[i])
      } else {
        # If no correlated indices found, just keep the current wavelength
        wavelengths_to_keep <- c(wavelengths_to_keep, colnames(cor_matrix)[i])
      }
    }
  }
  
  # Return the list of wavelengths to keep
  return(wavelengths_to_keep)
}

# Assuming dt is your pre-processed dataframe
# Ensure dt only contains numeric columns
dt_numeric <- dt %>% select_if(is.numeric)

# Apply the function to get the wavelengths to keep
wavelengths_to_keep <- reduce_correlated_wavelengths(dt_numeric)

# Create a dataframe with only the selected wavelengths
dt_reduced <- dt_numeric[, wavelengths_to_keep]

# View the reduced dataframe
print(dt_reduced)

column_names <- colnames(dt_reduced)




setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research/1_Experiment/1_Experiment_P/Spreadsheet/PLS/cv10")
dt <-read.delim("Project_P_PLS-cv10_correl_removed.txt")




# Load necessary libraries
library(dplyr)

# Function to find highly correlated wavelengths and retain only one from each group
reduce_correlated_wavelengths <- function(df, threshold = 0.99) {
  # Ensure df only contains numeric columns
  df_numeric <- df %>% select_if(is.numeric)
  
  # Compute the correlation matrix
  cor_matrix <- cor(df_numeric, use = "pairwise.complete.obs")
  
  # Create an empty list to store the wavelengths to keep
  wavelengths_to_keep <- c()
  
  # Create an empty vector to track wavelengths that have been grouped
  grouped_wavelengths <- rep(FALSE, ncol(cor_matrix))
  
  # Loop through each column (wavelength) in the correlation matrix
  for (i in 1:ncol(cor_matrix)) {
    if (!grouped_wavelengths[i]) {
      # Find indices of highly correlated wavelengths
      correlated_indices <- which(abs(cor_matrix[, i]) > threshold & abs(cor_matrix[, i]) < 1)
      
      if (length(correlated_indices) > 0) {
        # Mark these indices as grouped
        grouped_wavelengths[correlated_indices] <- TRUE
        grouped_wavelengths[i] <- TRUE
        # Keep the first wavelength in the group
        wavelengths_to_keep <- c(wavelengths_to_keep, colnames(cor_matrix)[i])
      } else {
        # If no correlated indices found, just keep the current wavelength
        wavelengths_to_keep <- c(wavelengths_to_keep, colnames(cor_matrix)[i])
      }
    }
  }
  
  # Return the list of wavelengths to keep
  return(wavelengths_to_keep)
}

# Assuming dt is your pre-processed dataframe
# Ensure dt only contains numeric columns
dt_numeric <- dt %>% select_if(is.numeric)

# Apply the function to get the wavelengths to keep
wavelengths_to_keep <- reduce_correlated_wavelengths(dt_numeric)

# Create a dataframe with only the selected wavelengths
dt_reduced <- dt_numeric[, wavelengths_to_keep]

# View the reduced dataframe
print(dt_reduced)

column_names <- colnames(dt_reduced)









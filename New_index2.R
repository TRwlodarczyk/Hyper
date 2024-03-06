#Testing bands


library(tidyverse)
library(viridis)

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research/1_Experiment/Analysis")
dt <-read.delim("Ahal_new_index_calc.txt")
dt2 <-read.delim("Ahal_new_index_calc2.txt")

#create simple ratio indicies:
{
# Function to calculate ratios and add them as new columns to the dataframe
calculate_and_append_ratios <- function(dt) {
  # Identify the wavelength columns based on their position
  wavelength_cols <- names(dt)[29:78]
  
  # Generate all unique pairs of these columns
  combinations <- combn(wavelength_cols, 2, simplify = FALSE)
  
  # Loop through each pair, calculate the ratio, and append it as a new column
  for (pair in combinations) {
    # Define the new column name based on the pair of wavelengths
    new_col_name <- paste0("Ratio_", pair[1], "_", pair[2])
    
    # Calculate the ratio and add it to the dataframe
    dt[[new_col_name]] <- dt[[pair[1]]] / dt[[pair[2]]]
  }
  
  return(dt)
}

# Apply the function to your dataframe
dt <- calculate_and_append_ratios(dt)

# Showing the structure of the updated dataframe to verify new columns
str(dt)

}


print(names(dt)[79])

#Correlate simple ratios with Zn
{
library(openxlsx)

# Initialize the dataframe to store correlations
cor_df <- data.frame(variable = character(), correlation = numeric(), stringsAsFactors = FALSE)

# Adjust the loop to correctly start from the first new index column
# Assuming your new indices start at column 79 and Zn concentration is correctly referenced
for (i in 79:ncol(dt)) {
  # Calculate the Spearman correlation
  correlation <- cor(dt[[i]], dt$Zn, method = "spearman", use = "complete.obs")
  
  # Store results
  cor_df[i-78, "variable"] <- names(dt)[i]
  cor_df[i-78, "correlation"] <- correlation
}

# Remove NA values
cor_df <- na.omit(cor_df)

# Write to an Excel file
write.xlsx(cor_df, file = "Correlation_Coefficients_NEW_INDICIES.xlsx")
}

#Create difference index
{

calculate_and_append_differences <- function(dt) {

  wavelength_cols <- names(dt)[29:78]
  combinations <- combn(wavelength_cols, 2, simplify = FALSE)
  
  for (pair in combinations) {
    
    new_col_name <- paste0("Diff_", pair[1], "_", pair[2])
    
    dt[[new_col_name]] <- dt[[pair[1]]] - dt[[pair[2]]]
  }
  
  return(dt)
}


dt <- calculate_and_append_differences(dt)

}

#Correlate difference indicies with Zn
{
  library(openxlsx)
  
  # Initialize the dataframe to store correlations
  cor_df <- data.frame(variable = character(), correlation = numeric(), stringsAsFactors = FALSE)
  
  # Adjust the loop to correctly start from the first new index column
  # Assuming your new indices start at column 79 and Zn concentration is correctly referenced
  for (i in 79:ncol(dt)) {
    # Calculate the Spearman correlation
    correlation <- cor(dt[[i]], dt$Zn, method = "spearman", use = "complete.obs")
    
    # Store results
    cor_df[i-78, "variable"] <- names(dt)[i]
    cor_df[i-78, "correlation"] <- correlation
  }
  
  # Remove NA values
  cor_df <- na.omit(cor_df)
  
  # Write to an Excel file
  write.xlsx(cor_df, file = "Correlation_Coefficients_Difference_Index_1data.xlsx")
}


####################More variables dataset dt2
#create simple ratio indicies:
{
{
  # Function to calculate ratios and add them as new columns to the dataframe
  calculate_and_append_ratios <- function(dt2) {
    # Identify the wavelength columns based on their position
    wavelength_cols <- names(dt2)[29:687]
    
    # Generate all unique pairs of these columns
    combinations <- combn(wavelength_cols, 2, simplify = FALSE)
    
    # Loop through each pair, calculate the ratio, and append it as a new column
    for (pair in combinations) {
      # Define the new column name based on the pair of wavelengths
      new_col_name <- paste0("Ratio_", pair[1], "_", pair[2])
      
      # Calculate the ratio and add it to the dataframe
      dt2[[new_col_name]] <- dt2[[pair[1]]] / dt2[[pair[2]]]
    }
    
    return(dt2)
  }
  
  # Apply the function to your dataframe
  dt2 <- calculate_and_append_ratios(dt2)
  
  # Showing the structure of the updated dataframe to verify new columns
  str(dt2)
  
}

library(openxlsx)

# Initialize the dataframe to store correlations
cor_df <- data.frame(variable = character(), correlation = numeric(), stringsAsFactors = FALSE)

# Adjust the loop to correctly start from the first new index column
# Assuming your new indices start at column 79 and Zn concentration is correctly referenced
for (i in 687:ncol(dt2)) {
  # Calculate the Spearman correlation
  correlation <- cor(dt2[[i]], dt2$Zn, method = "spearman", use = "complete.obs")
  
  # Store results
  cor_df[i-687, "variable"] <- names(dt2)[i]
  cor_df[i-687, "correlation"] <- correlation
}

# Remove NA values
cor_df <- na.omit(cor_df)

# Write to an Excel file
write.xlsx(cor_df, file = "Correlation_Coefficients_NEW_INDICIES2.xlsx")

}



####Create difference index with dt2


calculate_and_append_differences <- function(dt2) {
  
  wavelength_cols <- names(dt2)[29:687]
  combinations <- combn(wavelength_cols, 2, simplify = FALSE)
  
  for (pair in combinations) {
    
    new_col_name <- paste0("Diff_", pair[1], "_", pair[2])
    
    dt2[[new_col_name]] <- dt2[[pair[1]]] - dt2[[pair[2]]]
  }
  
  return(dt2)
}


dt2 <- calculate_and_append_differences(dt2)

library(openxlsx)

# Initialize the dataframe to store correlations
cor_df <- data.frame(variable = character(), correlation = numeric(), stringsAsFactors = FALSE)

# Adjust the loop to correctly start from the first new index column
# Assuming your new indices start at column 79 and Zn concentration is correctly referenced
for (i in 687:ncol(dt2)) {
  # Calculate the Spearman correlation
  correlation <- cor(dt[[i]], dt$Zn, method = "spearman", use = "complete.obs")
  
  # Store results
  cor_df[i-687, "variable"] <- names(dt2)[i]
  cor_df[i-687, "correlation"] <- correlation
}

# Remove NA values
cor_df <- na.omit(cor_df)

# Write to an Excel file
write.xlsx(cor_df, file = "Correlation_Coefficients_Difference_Index_ALL.xlsx")

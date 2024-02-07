#Spectrum tests with SD



library(tidyverse)
library(ggpubr)
library(pls)
library(tidyr)
library(dplyr)

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research/1_Experiment/Analysis")
dt <-read.delim("Ahall_V2.txt")



{
# Assuming data is already loaded and in the correct format

# Convert data to long format
long_data <- gather(dt, key = "sample", value = "reflectance", -Wavelength)

# Adjust the regular expressions to match the 'X' prefix in sample names
long_data <- long_data %>%
  mutate(treatment = gsub("X([A-Za-z]+)[0-9]+_A_T[0-9]+", "\\1", sample), 
         time = gsub("X[A-Za-z]+([0-9]+)_A_T[0-9]+", "\\1", sample)) 

# Remove rows with NA values if needed
long_data <- na.omit(long_data)

# Group by treatment and time, then summarise
summary_data <- long_data %>%
  group_by(treatment, time, Wavelength) %>%
  summarise(mean_reflectance = mean(reflectance, na.rm = TRUE),
            sd_reflectance = sd(reflectance, na.rm = TRUE))

# Print the results
print(summary_data)
}



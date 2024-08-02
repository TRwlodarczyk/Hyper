# Project P - Hyperspectral


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
dt <-read.delim("Project_P_pXRF_combined.txt")


dt1 <- dt %>% 
  filter(Sample != "QA")


# Remove NDs

{
  # Replace "ND" with 0 in columns 9 to 32
  for (i in 16:24) {
    dt1[, i] <- gsub(".*ND.*", 0, dt1[, i])
  }
  
  # Preserve columns 1 to 15
  dt1_preserved <- dt1[, 1:15]
  
  # Transform to dataframe
  dt1 <- as.data.frame(dt1)
  
  # Change character to numeric in columns 9 to 32
  dt1[, 16:24] <- sapply(dt1[, 16:24], as.numeric)
  
  # Combine preserved columns with modified columns
  dt1 <- cbind(dt1_preserved, dt1[, 16:24])
  }


#apply LODs
{
  

  dt1$Mn_PXRF[dt1$Mn_PXRF == 0] <- NA
  dt1$Fe_PXRF[dt1$Fe_PXRF == 0] <- NA
  dt1$Cu_PXRF[dt1$Cu_PXRF == 0] <- NA
  dt1$Zn_PXRF[dt1$Zn_PXRF == 0] <- NA

}




# Load necessary libraries
library(dplyr)

# Define your element columns and metadata columns
element_columns <- c("Cu_PXRF", "Zn_PXRF", "Mn_PXRF", "Fe_PXRF", "Cu_PXRF_unc", "Zn_PXRF_unc", "Mn_PXRF_unc", "Fe_PXRF_unc")
metadata_columns <- c("Substrate.RT", "PXRF_Scan", "Sample.Name", "Sample", "Treatment", "Measure", "Measure2", 
                      "Side", "Zn_in_solution_.μM.", "Plants", "Notes", "Hyperspectral_measured", "New_Scan", "Date", "File", "Material")

# Define the standard error function
se <- function(x) sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)))

# Compute the mean, SD, SE, min, max, and CV for each element within the specified groups and add metadata
dt2 <- dt1 %>%
  group_by(Sample.Name, Measure2, Zn_in_solution_.μM.) %>%
  summarise(across(all_of(element_columns), list(
    mean = ~ if (all(is.na(.x))) NA else mean(.x, na.rm = TRUE),
    median = ~ if (all(is.na(.x))) NA else median(.x, na.rm = TRUE),
    sd = ~ if (all(is.na(.x))) NA else sd(.x, na.rm = TRUE),
    se = ~ if (all(is.na(.x))) NA else se(.x),
    min = ~ if (all(is.na(.x))) NA else min(.x, na.rm = TRUE),
    max = ~ if (all(is.na(.x))) NA else max(.x, na.rm = TRUE),
    cv = ~ if (all(is.na(.x))) NA else sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)
  ), .names = "{.col}_{.fn}"), .groups = "drop") %>%
  # Ensure each group is represented once for the join to prevent duplicates
  left_join(
    dt1 %>%
      select(Sample.Name, Measure2, Zn_in_solution_.μM., all_of(metadata_columns)) %>%
      distinct(Sample.Name, Measure2, Zn_in_solution_.μM., .keep_all = TRUE),
    by = c("Sample.Name", "Measure2", "Zn_in_solution_.μM.")
  ) %>%
  # Reorder columns to place metadata columns at the beginning
  select(all_of(metadata_columns), everything())



write.xlsx(dt2, "Project_P_pXRF_summary.xlsx")




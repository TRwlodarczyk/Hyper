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
dt <-read.delim("Project_P_pXRF_HP_Hyper.txt")

plot(dt$Dry_leaf_area_cm2~dt$Zn_PXRF_mean)
plot(dt$DW_g~dt$Zn_PXRF_mean)
plot(dt$Substrate.RT~dt$Zn_PXRF_mean)
plot(dt$Substrate.RT~dt$DW_g)
plot(dt$Substrate.RT~dt$DW_g)
plot(dt$Leaf_Mass~dt$DW_g)

corr.test(dt$Dry_leaf_area_cm2, dt$Zn_PXRF_mean)
corr.test(dt$Leaf_Mass, dt$DW_g)

ggplot(dt, aes(x = Leaf_Mass, y = DW_g, color = Status)) + 
  geom_point(size=3) +  # adds the scatter plot points
  theme_minimal() + 
  labs(title = "ddd",
       x = "Leaf_Mass",
       y = "DW_g")

dt <- dt %>%
  mutate(wt = (Leaf_Mass - DW_g))









dt <- dt %>% 
  filter(Hyperspectral_measured == "Yes" &
           Leaf_Type == "pxrf")


colnames(dt)[148] #here the hyper cols starts 





# Create code to provide dataframe with correlation values (R-squared) between all cols
{
  
  selected_columns <- dt[, 148:2197]
r_squared_df <- data.frame(Column1 = character(),
                           Column2 = character(),
                           R_squared = numeric(),
                           stringsAsFactors = FALSE)


for (i in 1:ncol(selected_columns)) {
  for (j in (i + 1):ncol(selected_columns)) {

    model <- lm(selected_columns[, i] ~ selected_columns[, j])

    r_squared <- summary(model)$r.squared

    r_squared_df <- rbind(r_squared_df, data.frame(Column1 = colnames(selected_columns)[i],
                                                   Column2 = colnames(selected_columns)[j],
                                                   R_squared = r_squared,
                                                   stringsAsFactors = FALSE))
  }
}


head(r_squared_df)

#write.xlsx(r_squared_df, "Project_P_R_squared_spectra_corr.xlsx")

}



#Code to remove cols with high R-squared but leave one
{
columns_to_keep <- c()

# Identify highly correlated pairs
correlation_threshold <- 0.985

# Get unique pairs of correlated columns
correlated_pairs <- r_squared_df %>%
  filter(R_squared > correlation_threshold) %>%
  select(Column1, Column2)

# Create a vector to store already seen columns
seen_columns <- c()

# Iterate over each pair of correlated columns
for (i in 1:nrow(correlated_pairs)) {
  col1 <- correlated_pairs$Column1[i]
  col2 <- correlated_pairs$Column2[i]
  
  # Check if either column is already seen (i.e., already added to keep)
  if (!(col1 %in% seen_columns) && !(col2 %in% seen_columns)) {
    # If neither has been seen, keep one (e.g., col1)
    columns_to_keep <- c(columns_to_keep, col1)
    seen_columns <- c(seen_columns, col1, col2)
  } else if (col1 %in% seen_columns && !(col2 %in% seen_columns)) {
    # If col1 has been seen, keep col2
    seen_columns <- c(seen_columns, col2)
  } else if (col2 %in% seen_columns && !(col1 %in% seen_columns)) {
    # If col2 has been seen, keep col1
    seen_columns <- c(seen_columns, col1)
  }
}

# Add any columns that were not part of the correlated pairs
all_columns <- colnames(dt)[148:2197]
columns_to_keep <- unique(c(columns_to_keep, setdiff(all_columns, seen_columns)))

# Filter the original dataset to keep only the selected columns
reduced_dataset <- dt[, columns_to_keep]

# View the resulting dataset
head(reduced_dataset)
}


#Correlation
{
  
dt <-read.delim("Project_P_correlation.txt")

head(dt)

# Remove the first column if it's just wavelengths
data_melted <- melt(dt, id.vars = "Variables")  # Replace "wavelengths" with the actual column name if it's different

# Create the heatmap
g <- ggplot(data_melted, aes(x = variable, y = Variables)) +
  geom_tile(aes(fill = value), color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap", x = "Variables", y = "Wavelengths")
 
#ggsave("corrplot.pdf", plot = g, width = 25, height = 25, units = "in") 
  



# Load the dataset
dt <- read.delim("Project_P_correlation.txt")

# Convert the data frame to a matrix (assuming the first column contains wavelengths)
data_matrix <- as.matrix(dt[, -1])  # Exclude the first column (wavelengths)

# Set row names (wavelengths)
rownames(data_matrix) <- dt[[1]]  # Set the first column as row names

# Create heatmap
heatmap(data_matrix, 
        Colv = NA, 
        Rowv = NA, 
        scale = "none", 
        col = colorRampPalette(c("blue", "white", "red"))(256), 
        main = "Correlation Heatmap")




library(pheatmap)

# Convert the data frame to a matrix (excluding the first column)
data_matrix <- as.matrix(dt[, -1])  # Exclude the first column (wavelengths)

# Set row names (wavelengths)
rownames(data_matrix) <- dt[[1]]  # Set the first column as row names

# Create a custom function to select every 100th label
select_labels <- function(labels, interval) {
  return(labels[seq(1, length(labels), by = interval)])
}

# Create the heatmap without clustering and with custom axis labels
pheatmap(data_matrix, 
         cluster_rows = FALSE, 
         cluster_cols = FALSE, 
         scale = "none", 
         color = colorRampPalette(c("navyblue", "white", "darkred"))(256), 
         main = "Correlation Heatmap",
         fontsize_row = 10,    # Adjust row label font size if needed
         fontsize_col = 10,    # Adjust column label font size if needed
         labels_row = select_labels(rownames(data_matrix), 100),  # Custom row labels
         labels_col = select_labels(colnames(data_matrix), 100))  # Custom c
}

#ggplot corr spectrum Zn

{
  dt$Variables <- as.numeric(as.character(dt$Variables)) 
  ggplot(dt, aes(x = Variables, y = Zn_PXRF_mean)) + 
    geom_line()+
    theme_minimal() + 
    labs(title = "Zn2",
         x = "Wavelength",
         y = "Zinc mean")  


}



ggplot(dt, aes(x = Variables, y = Zn_PXRF_mean)) + 
  geom_line()+
  theme_minimal() + 
  labs(title = "Zn2",
       x = "Wavelength",
       y = "Zinc mean")  












plot(dt$F5.to.Fm~dt$Zn_PXRF_mean) # corr spearman -0.521




















##### Plots

colnames(dt)[149] #here the hyper cols starts 
colnames(dt)[148] #here the hyper cols starts 


# Rename columns to remove the "X" prefix
colnames(dt)[149:2198] <- gsub("^X", "", colnames(dt)[149:2198])

# Convert the renamed columns to numeric
dt <- dt %>%
  mutate(across(149:2198, as.numeric))

dt_main_long <- dt %>%
  pivot_longer(cols = 149:2198, names_to = "Waveband", values_to = "Reflectance") # I HAD TO CHANGE TO WAVEBAND BECAUSE I HAVE COL OF WAVELENGTH ALREDY


# Check the structure of dt_main_long
str(dt[1149])

str(dt_main_long[149])

dt_main_long[,149] <- sapply(dt_main_long[,149],as.numeric)# because the x axis was not in the order


harvest2 <- ggplot(dt_main_long, aes(x = Waveband, y = Reflectance, group = Sample_ID, color = Status)) + 
  #geom_ribbon(aes(ymin = Reflectance - SD, ymax = Reflectance + SD, fill = Treatment), alpha = 0.2, colour=NA) + # Adjusted alpha for SD shadow
  geom_line(linewidth=0.5) +   # 
  geom_point(size=0.2, alpha = 0.5) +  #
  theme_minimal() + 
  labs(title = "Harvest measurements",
       x = "Wavelength (nm)",
       y = "Reflectance") +
  scale_color_viridis_d() + # 
  scale_fill_viridis_d()    #



harvest3 <- ggplot(dt_main_long, aes(x = Waveband, y = Reflectance, group = Sample_ID, color = Zn_PXRF_mean)) + 
  #geom_ribbon(aes(ymin = Reflectance - SD, ymax = Reflectance + SD, fill = Treatment), alpha = 0.2, colour=NA) + # Adjusted alpha for SD shadow
  geom_line(linewidth=0.5) +   # 
  geom_point(size=0.2, alpha = 0.5) +  #
  theme_minimal() + 
  labs(title = "Harvest measurements",
       x = "Wavelength (nm)",
       y = "Reflectance") +
  scale_color_gradient(low = "lightgrey", high = "darkred") + # Light grey to red gradient
  scale_fill_viridis_d()    #





############### Group plants based on characteristics
###############
setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research/1_Experiment/1_Experiment_P/Spreadsheet")
dt <-read.delim("Project_P_pXRF_HP_Hyper.txt")

dt <- dt %>%
  mutate(VI497_517 = (X497- X517) / (X497 + X517))


ggplot(dt, aes(x = VI497_517, y = Zn_PXRF_mean, group = Status, color = Status)) + 
  geom_point(size=3, alpha = 0.5) +  #
  theme_minimal() 

### NEED A CLUSTER HERE OR PCA



############## 
##############


#### Index development for Project_P_PLS-cv20-4 dataset where we still have 1500 and 2000 wavebands. 
######## TOO LONG TO FINISH

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research/1_Experiment/1_Experiment_P/Spreadsheet/PLS/cv20")
dt <-read.delim("Project_P_PLS-cv20-4.txt")



colnames(dt)[149] #here the hyper cols starts 

# Get column names for the relevant columns (149 to 296)
relevant_columns <- names(dt)[149:482]

# Function to calculate the index for a pair of columns
calculate_index <- function(col1, col2) {
  (dt[[col1]] - dt[[col2]]) / (dt[[col1]] + dt[[col2]])
}

# Create a dataframe to hold the names of new indices and their R-squared values
results <- data.frame(Index = character(), R_squared = numeric(), stringsAsFactors = FALSE)

# Iterate over the pairs of columns to create new indices and calculate R-squared values
for (i in 1:(length(relevant_columns) - 1)) {
  for (j in (i + 1):length(relevant_columns)) {
    col1 <- relevant_columns[i]
    col2 <- relevant_columns[j]
    new_col_name <- paste(col1, col2, sep = "_")
    dt[[new_col_name]] <- calculate_index(col1, col2)
    
    # Calculate the R-squared value
    model <- lm(dt[[new_col_name]] ~ dt$Zn_PXRF_mean, data = dt)
    r_squared <- summary(model)$r.squared
    
    # Add the result to the results dataframe
    results <- rbind(results, data.frame(Index = new_col_name, R_squared = r_squared))
  }
}

# View the results dataframe
print(results)


#########################
##########################
#########################
### Leave 1 band every 10 bands (every 10th band)



#### Index development for Project_P_PLS-cv20-4 dataset where we still have 1500 and 2000 wavebands. 
######## TOO LONG TO FINISH

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research/1_Experiment/1_Experiment_P/Spreadsheet/PLS")
dt <-read.delim("Project_P_pXRF_HP_Hyper.txt")



colnames(dt)[149]

columns_to_keep <- seq(149, 2198, by = 10)
dt_reduced <- dt[, c(1:148, columns_to_keep)]
str(dt_reduced)

dt <- dt_reduced


#some plot

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research/1_Experiment/1_Experiment_P/Spreadsheet/PLS")
dt <-read.delim("Project_P_pXRF_HP_Hyper.txt")



dt <- dt %>%
  mutate(VI550_728 = (X550 - X728) / (X550 + X728))

dt1 <- subset(dt, Zn_CV_filter=="Yes")
corr.test(dt$VI550_728, dt$Zn_PXRF_mean) # R-sq = 0.41





# Create the plot

ggplot(dt, aes(x = Zn_PXRF_mean, y = VI550_728, color = Status)) + 
  geom_point(size=3) +  # adds the scatter plot points
  theme_minimal() + 
  labs(title = "",
       x = "Zn conc in leaf",
       y = "VI550_728")




dt <- dt %>%
  mutate(VI497_517 = (X497 - X517) / (X497 + X517))
corr.test(dt$VI497_517, dt$Zn_PXRF_mean) # R-sq = 0.45

ggplot(dt, aes(x = Zn_PXRF_mean, y = VI497_517, color = Status)) + 
  geom_point(size=3) +  # adds the scatter plot points
  theme_minimal() + 
  labs(title = "",
       x = "Zn conc in leaf",
       y = "VI497_517")

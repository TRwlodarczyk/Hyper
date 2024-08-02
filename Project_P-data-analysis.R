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

write.xlsx(r_squared_df, "Project_P_R_squared_spectra_corr.xlsx")

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
 
ggsave("corrplot.pdf", plot = g, width = 25, height = 25, units = "in") 
  



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








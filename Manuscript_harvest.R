#TW
#Halleri hyper


library(tidyverse)
library(ggpubr)
library(pls)
library(openxlsx)
library(stringr)


setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research/1_Experiment/Analysis")
dt <-read.delim("Area_harvest_spec.txt")
dt2 <-read.delim("Ahal_pXRF-Feb24.txt")
dt3 <-read.delim("All_harvest_pXRF.txt")
dt2[,5:27] <- sapply(dt2[,5:27],as.numeric)


dt3 <- dt3 %>%
  filter(str_detect(ID, "^av_"))

dt_sd <- dt3 %>%
  filter(str_detect(ID, "_sd")) %>%
  mutate(ID = str_replace(ID, "_sd", "")) %>%
  pivot_longer(cols = 29:2179, names_to = "Wavelength", values_to = "SD")

dt_main_long <- dt3 %>%
  filter(!str_detect(ID, "_sd")) %>%
  pivot_longer(cols = 29:2179, names_to = "Wavelength", values_to = "Reflectance")

dt_combined <- dt_main_long %>%
  left_join(select(dt_sd, ID, Wavelength, SD), by = c("ID", "Wavelength"))

dt_combined <- dt_combined %>%
  mutate(Wavelength = str_replace_all(Wavelength, "X", ""))

dt_combined[,"Wavelength"] <- sapply(dt_combined[,"Wavelength"],as.numeric)

dt_combined <- dt_combined %>%
  filter(Reflectance >= 0 & Wavelength > 400 & Wavelength <= 2450)

#write.table(dt_combined, file="C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research/1_Experiment/Analysis/Ahal_harvest_long.csv", sep=",", row.names = F)


{
long_data <- dt %>%
  pivot_longer(cols = -Wavelength, names_to = "ID", values_to = "Reflectance") %>%
  # Generate a flag for SD columns and separate Time from the rest of the ID
  mutate(SD_flag = str_detect(ID, "_sd$"),
         ID = str_replace(ID, "_sd$", ""),
         Treatment = str_replace(ID, "T\\d+", "T9")) %>%
  # Spread the SD_flag to wide format to separate Reflectance and SD
  pivot_wider(names_from = SD_flag, values_from = Reflectance, 
              names_prefix = "Value_") %>%
  # Rename the columns
  rename(Reflectance = Value_FALSE, SD = Value_TRUE) %>%
  # Select the desired order of columns
  select(Treatment, Wavelength, Reflectance, SD)

# View the head of the reshaped dataframe to confirm it's correct

long_data <- long_data %>%
  filter(Treatment %in% c("averC", "averZn1", "averZn2"))

#to see how it looks without outlier Zn16 in the average
long_data <- long_data %>%
  filter(Treatment %in% c("averC", "averZn1_2", "averZn2"))

long_data <- long_data %>%
  filter(Reflectance >= 0)

long_data <- long_data %>%
  filter(Wavelength >= 450, Wavelength < 700)


ggplot(long_data, aes(x = Wavelength, y = Reflectance, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = Reflectance - SD, ymax = Reflectance + SD, fill = Treatment), alpha = 0.15, colour=NA) +
  #geom_point(size=0.3) +  # adds the scatter plot points
  geom_line(size=0.8) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "T9",
       x = "Wavelength (nm)",
       y = "Reflectance") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d() 
  #geom_vline(xintercept = 554, linetype = "dashed", color = "darkgreen", size=1) +
  #geom_vline(xintercept = 671, linetype = "dashed", color = "darkred", size=1) 



##just to compare aver with and without 6Zn1

long_data <- long_data %>%
  filter(Treatment %in% c("averZn1_2", "averZn1"))


ggplot(long_data, aes(x = Wavelength, y = Reflectance, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = Reflectance - SD, ymax = Reflectance + SD, fill = Treatment), alpha = 0.15, colour=NA) +
  #geom_point(size=0.3) +  # adds the scatter plot points
  geom_line(size=0.8) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "T9",
       x = "Wavelength (nm)",
       y = "Reflectance") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d() 



long_data <- long_data %>%
  filter(Treatment %in% c("X1Zn1_A_T9", "X6Zn1_A_T9", "X2Zn1_A_T9", "X3Zn1_A_T9", "X4Zn1_A_T9", "X5Zn1_A_T9", "X4Zn2_A_T9"))


{
  
  ggplot(data=dt, aes(x=Wavelength)) +
    geom_line(aes(y=X8C_A_T9), color="darkgreen") +
    geom_line(aes(y=X9C_A_T9), color="darkgreen") +
    geom_line(aes(y=X10C_A_T9), color="darkgreen") +
    geom_line(aes(y=X11C_A_T9), color="darkgreen") +
    geom_line(aes(y=X12C_A_T9), color="darkgreen") +
    geom_line(aes(y=X13C_A_T9), color="darkgreen") +
    geom_line(aes(y=X14C_A_T9), color="darkgreen") +
    geom_line(aes(y=X15C_A_T9), color="darkgreen") +
    geom_line(aes(y=X1Zn1_A_T9), color="orange") +  
    geom_line(aes(y=X2Zn1_A_T9), color="orange") + 
    geom_line(aes(y=X3Zn1_A_T9), color="orange") + 
    geom_line(aes(y=X4Zn1_A_T9), color="orange") + 
    geom_line(aes(y=X5Zn1_A_T9), color="orange") + 
    geom_line(aes(y=X6Zn1_A_T9), color="orange") + 
    geom_line(aes(y=X1Zn2_A_T9), color="darkred") +
    geom_line(aes(y=X2Zn2_A_T9), color="darkred") +
    geom_line(aes(y=X3Zn2_A_T9), color="darkred") +
    geom_line(aes(y=X4Zn2_A_T9), color="darkred") +
    geom_line(aes(y=X5Zn2_A_T9), color="darkred") +
    geom_line(aes(y=X6Zn2_A_T9), color="darkred") +
    scale_x_continuous(limits = c(350, 2500), breaks = c(seq(350, 2500, by = 175))) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    theme_classic() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=11),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          axis.text.y = element_text(size=11),
          legend.key.size = unit(1, "lines"),
          legend.text = element_text(size = 13.5), 
          legend.title = element_text(size=15, face = "bold"))+
    guides(color = guide_legend(override.aes = list(size = 3.5)),
           shape = guide_legend(override.aes = list(size = 3.5))) +
    ylab("Reflectance")+
    xlab("Wavelength") + 
    ggtitle("Green - C, Orange - Zn1, Red - Zn2")
  
  
  
  filter(ID %in% c("1Zn1_A_T9", "2Zn1_A_T9", "3Zn1_A_T9", "4Zn1_A_T9", "5Zn1_A_T9", "6Zn1_A_T9",
                   "8C_A_T9", "9C_A_T9", "10C_A_T9", "11C_A_T9", "1Zn2_A_T9", "2Zn2_A_T9",
                   "3Zn2_A_T9", "4Zn2_A_T9", "5Zn2_A_T9", "6Zn2_A_T9", "12C_A_T9", "13C_A_T9",
                   "14C_A_T9", "15C_A_T9"))
  
  
  
  }

}

#Create summary statistics
{
# Define the standard error function
se <- function(x) { sd(x, na.rm = TRUE) / sqrt(length(na.omit(x))) }

# Revised code
dt2_av <- dt2 %>%
  group_by(ID) %>%
  select(5:27) %>%
  summarise(across(everything(), list(
    min = ~min(.x, na.rm = TRUE),
    max = ~max(.x, na.rm = TRUE),
    mean = ~mean(.x, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE),
    se = ~se(.x) # Use the standard error function
  ), .names = "{.col}_{.fn}"), .groups = "drop") %>%
  pivot_longer(cols = -ID, names_to = "Parameter_Stat", values_to = "Value") %>%
  separate(Parameter_Stat, into = c("Parameter", "Stat"), sep = "_", extra = "merge") %>%
  pivot_wider(names_from = Stat, values_from = Value)


write.xlsx(dt2_av, file = "Summmary_pXRF2.xlsx")
}



#Correlation dt3
{

dt3 <- dt3 %>%
  slice(61:100)
  
  columns_to_remove <- c(29:78, 2130:2179)
  dt3 <- dt3 %>%
    select(-all_of(columns_to_remove))
  
# Assuming dt3 is your dataset and it contains a column for Zn concentration correctly named "Zn"
library(openxlsx)

# Create an empty dataframe to store the correlation coefficients
# Adjust the size of the dataframe based on actual columns of interest (2170 = 2175 - 5)
cor_df <- data.frame(variable = character(2173), correlation = numeric(2173), stringsAsFactors = FALSE)

# Loop through each column from 6 (indices values) to 2175 (spectral reflectance)
for (i in 7:2179) {
  # Calculate the Spearman correlation between column i and "Zn_concentration"
  correlation <- cor(dt3[, i], dt3$Zn, method = "spearman", use = "complete.obs")
  
  # Store the variable name and correlation coefficient in the dataframe
  cor_df[i - 5, "variable"] <- names(dt3)[i]
  cor_df[i - 5, "correlation"] <- correlation
}

# Remove rows with NA (in case some correlations could not be computed)
cor_df <- na.omit(cor_df)

# Optionally, write the correlation coefficients to an Excel file
write.xlsx(cor_df, file = "Correlation_Coefficients3.xlsx")

}

#Spectrum view
{
  #####
  dt3 <- dt3 %>%
    filter(str_detect(ID, "^av_"))
  
  dt_sd <- dt3 %>%
    filter(str_detect(ID, "_sd")) %>%
    mutate(ID = str_replace(ID, "_sd", "")) %>%
    pivot_longer(cols = 29:2179, names_to = "Wavelength", values_to = "SD")

  dt_main_long <- dt3 %>%
    filter(!str_detect(ID, "_sd")) %>%
    pivot_longer(cols = 29:2179, names_to = "Wavelength", values_to = "Reflectance")

  dt_combined <- dt_main_long %>%
    left_join(select(dt_sd, ID, Wavelength, SD), by = c("ID", "Wavelength"))
  

  #write.xlsx(dt_combined, "dt_combined.xlsx", rowNames = FALSE)
  

 # dt_long <- dt_long %>%
#    mutate(Reflectance = if_else(!is.na(SD), NA_real_, Reflectance))
  ######
  
  dt_combined <- dt_combined %>%
    mutate(Wavelength = str_replace_all(Wavelength, "X", ""))
  
  dt_combined[,"Wavelength"] <- sapply(dt_combined[,"Wavelength"],as.numeric)
  
  dt_combined <- dt_combined %>%
    filter(Reflectance >= 0)
  dt_combined <- dt_combined %>%
    filter(Reflectance >= 0 & Wavelength > 400 & Wavelength <= 2450)
  
  harvest2 <- ggplot(dt_combined, aes(x = Wavelength, y = Reflectance, group = ID, color = Treatment)) + 
    geom_ribbon(aes(ymin = Reflectance - SD, ymax = Reflectance + SD, fill = Treatment), alpha = 0.2, colour=NA) + # Adjusted alpha for SD shadow
    geom_line(linewidth=0.5) +   # 
    geom_point(size=0.2, alpha = 0.5) +  #
    theme_minimal() + 
    labs(title = "Harvest measurements",
         x = "Wavelength (nm)",
         y = "Reflectance") +
    scale_color_viridis_d() + # 
    scale_fill_viridis_d()    #
  
  library(viridis) # For viridis color scales
  
  harvest2 <- ggplot(dt_combined, aes(x = Wavelength, y = Reflectance, group = ID, color = Treatment)) + 
    geom_ribbon(aes(ymin = Reflectance - SD, ymax = Reflectance + SD, fill = Treatment), alpha = 0.2, colour=NA) + # Adjusted alpha for SD shadow
    geom_line(linewidth=0.5) +   # 
    geom_point(size=0.2, alpha = 0.5) +  #
    geom_vline(xintercept = 606, linetype = "dashed", color = "red") + # Add a vertical dashed red line at 606 nm
    geom_vline(xintercept = 512, linetype = "dashed", color = "red") + # Add a vertical dashed red line at 606 nm
    geom_vline(xintercept = 536, linetype = "dashed", color = "red") + # Add a vertical dashed red line at 606 nm
    geom_vline(xintercept = 718, linetype = "dashed", color = "red") + # Add a vertical dashed red line at 606 nm
    geom_vline(xintercept = 831, linetype = "dashed", color = "red") + # Add a vertical dashed red line at 606 nm
    geom_vline(xintercept = 2235, linetype = "dashed", color = "red") + # Add a vertical dashed red line at 606 nm
    geom_vline(xintercept = 1881, linetype = "dashed", color = "red") + # Add a vertical dashed red line at 606 nm
    geom_vline(xintercept = 1420, linetype = "dashed", color = "red") + # Add a vertical dashed red line at 606 nm
    theme_minimal() + 
    labs(title = "Harvest measurements",
         x = "Wavelength (nm)",
         y = "Reflectance") +
    scale_color_viridis_d() + # 
    scale_fill_viridis_d()    #
  
  
  
  
  harvest2 <- ggplot(dt_combined, aes(x = Wavelength, y = Reflectance, group = ID, color = Treatment)) + 
    geom_ribbon(aes(ymin = Reflectance - SD, ymax = Reflectance + SD, fill = Treatment), alpha = 0.2, colour=NA) + # Adjusted alpha for SD shadow
    geom_line(linewidth=0.5) +   # 
    geom_point(size=0.2, alpha = 0.5) +  #
    geom_vline(xintercept = 514, linetype = "dashed", color = "red") + # Add a vertical dashed red line at 606 nm
    geom_vline(xintercept = 518, linetype = "dashed", color = "red") + # Add a vertical dashed red line at 606 nm
    geom_vline(xintercept = 707, linetype = "dashed", color = "red") + # Add a vertical dashed red line at 606 nm
    geom_vline(xintercept = 523, linetype = "dashed", color = "red") + # Add a vertical dashed red line at 606 nm
    geom_vline(xintercept = 536, linetype = "dashed", color = "red") + # Add a vertical dashed red line at 606 nm
    geom_vline(xintercept = 1410, linetype = "dashed", color = "red") + # Add a vertical dashed red line at 606 nm
    geom_vline(xintercept = 703, linetype = "dashed", color = "red") + # Add a vertical dashed red line at 606 nm
    geom_vline(xintercept = 1397, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 2420, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 1420, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 2347, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 2420, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 1880, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 1424, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 1506, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 1417, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 1539, linetype = "dashed", color = "red") +# Add a vertical dashed red line at 606 nm
    theme_minimal() + 
    labs(title = "Harvest measurements",
         x = "Wavelength (nm)",
         y = "Reflectance") +
    scale_color_viridis_d() + # 
    scale_fill_viridis_d()    #
  
  

  library(plotly)
  
  # Convert the ggplot object to a plotly object
  harvest2_plotly <- ggplotly(harvest2)
  
  # Display the interactive plot
  harvest2_plotly
  
  library(htmlwidgets)
  
  # Save your Plotly plot to an HTML file
  saveWidget(harvest2_plotly, "harvest2_plotly2.html")
  
  
}


#PCA
{
  
  library(tidyverse)
  library(factoextra) # For PCA and fviz_cluster
  library(cluster)    # For clustering
  
  # Step 1: Reshape the data for PCA
  dt_pca_ready <- dt_combined %>%
    select(ID, Wavelength, Reflectance) %>%
    spread(key = Wavelength, value = Reflectance)
  
  # Remove any NA values or impute them as necessary
  # For simplicity, here we'll remove them
  dt_pca_ready <- na.omit(dt_pca_ready)
  
  # Step 2: Perform PCA on the Reflectance values
  pca_results <- prcomp(dt_pca_ready[,-1], scale. = TRUE) # Exclude ID column for PCA
  
  # Step 3: Cluster PCA results into 3 clusters
  set.seed(123) # For reproducibility
  clusters <- kmeans(pca_results$x, centers = 3)
  
  # Visualize PCA results with clusters
  fviz_cluster(list(data = pca_results$x, cluster = clusters$cluster))

  
  # Assuming pca_results and clusters are already computed
  # And dt_pca_ready contains the original IDs
  
  # Convert PCA results to a data frame
  pca_df <- as.data.frame(pca_results$x)
  
  # Add cluster assignments to pca_df
  pca_df$Cluster <- clusters$cluster
  
  # Ensure the row order in pca_df matches dt_pca_ready to correctly align IDs
  pca_df$ID <- dt_pca_ready$ID
  
  # Now, let's plot with ggplot2, including IDs
  library(ggplot2)
  
  ggplot(pca_df, aes(x = PC1, y = PC2, label = ID, color = as.factor(Cluster))) +
    geom_point() +  # Add points for PCA results
    geom_text(aes(label = ID), vjust = 2, hjust = 0.5, size = 3, check_overlap = TRUE) +  # Annotate points with IDs
    scale_color_manual(values = c("red", "blue", "green")) +  # Customize cluster colors if desired
    theme_minimal() +
    labs(title = "PCA Clusters with IDs", x = "Principal Component 1", y = "Principal Component 2") +
    theme(legend.title = element_blank())  # Hide the legend title if desired
  
  #it suggest that spectra can distinguish between high and low Zn, but not between high and medium
  
}





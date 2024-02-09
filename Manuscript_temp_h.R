#Tomasz
#Final Manusctipt



library(tidyverse)
library(ggpubr)
library(pls)

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research/1_Experiment/Analysis")
dt <-read.delim("Area_spectrum_temp_2.2.24.txt")
dt <-read.delim("Area_spectrum_temporal_av.txt")


#calculate the peaks of green (the highest value) - most of the plants have peak in 554.
{
  
  subset_data <- subset(dt, Wavelength >= 500 & Wavelength <= 560)
  max_wavelengths <- numeric(ncol(subset_data) - 1)
  
  for(i in 2:ncol(subset_data)) {
    max_index <- which.max(subset_data[, i])
    max_wavelengths[i - 1] <- subset_data$Wavelength[max_index]
  }
  
  # Print the results
  names(max_wavelengths) <- colnames(subset_data)[-1]
  max_wavelengths
  
  wavelength_counts <- table(max_wavelengths)
  wavelength_counts #67 peaks in 554
  
}

#calculate the peaks of red (the lowest value)  - most of the plants have peak in 671
{
  
  subset_data <- subset(dt, Wavelength >= 640 & Wavelength <= 700)
  min_wavelengths <- numeric(ncol(subset_data) - 1)
  
  for(i in 2:ncol(subset_data)) {
    min_index <- which.min(subset_data[, i])
    min_wavelengths[i - 1] <- subset_data$Wavelength[min_index]
  }
  
  # Print the results
  names(min_wavelengths) <- colnames(subset_data)[-1]
  min_wavelengths
  
  wavelength_counts <- table(min_wavelengths)
  wavelength_counts #67 peaks in 554
  
} 







T0 <- ggplot(data=dt, aes(x=Wavelength)) +
  geom_ribbon(aes(ymin = CT0 - CT0_sd, ymax = CT0 + CT0_sd, fill = "green"), alpha = 0.15, colour=NA) +
  geom_line(aes(y=CT0), color="green") +
  geom_ribbon(aes(ymin = Zn1T0 - Zn1T0_sd, ymax = Zn1T0 + Zn1T0_sd, fill = "orange"), alpha = 0.15, colour=NA) +
  geom_line(aes(y=Zn1T0), color="orange") +
  geom_ribbon(aes(ymin = Zn2T0 - Zn2T0_sd, ymax = Zn2T0 + Zn2T0_sd, fill = "red"), alpha = 0.15, colour=NA) +
  geom_line(aes(y=Zn2T0), color="red") +
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
        legend.title = element_text(size=15, face = "bold")) +
  guides(color = guide_legend(override.aes = list(size = 3.5)),
         shape = guide_legend(override.aes = list(size = 3.5))) +
  ylab("Reflectance") +
  xlab("Wavelength") + 
  ggtitle("Green-CT0, Orange - Zn1T0, Red - Zn2T07") +
  scale_fill_manual(values = c("green", "orange", "red"))


library(tidyverse)


# Pivot the dataset
# Use pivot_longer to go to a long format
long_data <- dt %>%
  pivot_longer(cols = -Wavelength, names_to = "ID", values_to = "Reflectance") %>%
  # Generate a flag for SD columns and separate Time from the rest of the ID
  mutate(SD_flag = str_detect(ID, "_sd$"),
         ID = str_replace(ID, "_sd$", ""),
         Time = str_extract(ID, "T\\d+"),
         Treatment = str_replace(ID, "T\\d+", "")) %>%
  # Spread the SD_flag to wide format to separate Reflectance and SD
  pivot_wider(names_from = SD_flag, values_from = Reflectance, 
              names_prefix = "Value_") %>%
  # Rename the columns
  rename(Reflectance = Value_FALSE, SD = Value_TRUE) %>%
  # Select the desired order of columns
  select(Treatment, Time, Wavelength, Reflectance, SD)

# View the head of the reshaped dataframe to confirm it's correct
head(long_data)


#filter to remove negative values
long_data <- long_data %>%
  filter(Reflectance >= 0)

T0_data <- long_data %>%
  filter(Time == "T0")
T1_data <- long_data %>%
  filter(Time == "T1")
T3_data <- long_data %>%
  filter(Time == "T3")
T4_data <- long_data %>%
  filter(Time == "T4")
T5_data <- long_data %>%
  filter(Time == "T5")
T6_data <- long_data %>%
  filter(Time == "T6")
T7_data <- long_data %>%
  filter(Time == "T7")
T8_data <- long_data %>%
  filter(Time == "T8")
T9_data <- long_data %>%
  filter(Time == "T9")


# Plot
T0 <- ggplot(T0_data, aes(x = Wavelength, y = Reflectance, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = Reflectance - SD, ymax = Reflectance + SD, fill = Treatment), alpha = 0.15, colour=NA) +
  #geom_point(size=0.3) +  # adds the scatter plot points
  geom_line(size=0.55) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "T0",
       x = "Wavelength (nm)",
       y = "Reflectance") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()    # For discrete fill scale based on the Treatment

T1 <- ggplot(T1_data, aes(x = Wavelength, y = Reflectance, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = Reflectance - SD, ymax = Reflectance + SD, fill = Treatment), alpha = 0.15, colour=NA) +
  #geom_point(size=0.3) +  # adds the scatter plot points
  geom_line(size=0.55) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "T1",
       x = "Wavelength (nm)",
       y = "Reflectance") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()    # For discrete fill scale based on the Treatment

T3 <- ggplot(T3_data, aes(x = Wavelength, y = Reflectance, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = Reflectance - SD, ymax = Reflectance + SD, fill = Treatment), alpha = 0.15, colour=NA) +
  #geom_point(size=0.3) +  # adds the scatter plot points
  geom_line(size=0.55) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "T3",
       x = "Wavelength (nm)",
       y = "Reflectance") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()    # For discrete fill scale based on the Treatment

T4 <- ggplot(T4_data, aes(x = Wavelength, y = Reflectance, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = Reflectance - SD, ymax = Reflectance + SD, fill = Treatment), alpha = 0.15, colour=NA) +
  #geom_point(size=0.3) +  # adds the scatter plot points
  geom_line(size=0.55) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "T4",
       x = "Wavelength (nm)",
       y = "Reflectance") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()    # For discrete fill scale based on the Treatment

T5 <- ggplot(T5_data, aes(x = Wavelength, y = Reflectance, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = Reflectance - SD, ymax = Reflectance + SD, fill = Treatment), alpha = 0.15, colour=NA) +
  #geom_point(size=0.3) +  # adds the scatter plot points
  geom_line(size=0.55) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "T5",
       x = "Wavelength (nm)",
       y = "Reflectance") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()    # For discrete fill scale based on the Treatment

T6 <- ggplot(T6_data, aes(x = Wavelength, y = Reflectance, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = Reflectance - SD, ymax = Reflectance + SD, fill = Treatment), alpha = 0.15, colour=NA) +
  #geom_point(size=0.3) +  # adds the scatter plot points
  geom_line(size=0.55) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "T6",
       x = "Wavelength (nm)",
       y = "Reflectance") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()    # For discrete fill scale based on the Treatment


T7 <- ggplot(T7_data, aes(x = Wavelength, y = Reflectance, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = Reflectance - SD, ymax = Reflectance + SD, fill = Treatment), alpha = 0.15, colour=NA) +
  #geom_point(size=0.3) +  # adds the scatter plot points
  geom_line(size=0.55) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "T7",
       x = "Wavelength (nm)",
       y = "Reflectance") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()    # For discrete fill scale based on the Treatment


T8 <- ggplot(T8_data, aes(x = Wavelength, y = Reflectance, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = Reflectance - SD, ymax = Reflectance + SD, fill = Treatment), alpha = 0.15, colour=NA) +
  #geom_point(size=0.3) +  # adds the scatter plot points
  geom_line(size=0.55) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "T8",
       x = "Wavelength (nm)",
       y = "Reflectance") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()    # For discrete fill scale based on the Treatment


T9 <- ggplot(T9_data, aes(x = Wavelength, y = Reflectance, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = Reflectance - SD, ymax = Reflectance + SD, fill = Treatment), alpha = 0.15, colour=NA) +
  #geom_point(size=0.3) +  # adds the scatter plot points
  geom_line(size=0.55) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "T9",
       x = "Wavelength (nm)",
       y = "Reflectance") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()    # For discrete fill scale based on the Treatment




ggarrange(T0, T1, T3, T4, T5, T6, T7, T8, T9, ncol = 3, nrow = 3, 
          common.legend = TRUE)

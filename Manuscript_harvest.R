#TW
#Halleri hyper


library(tidyverse)
library(ggpubr)
library(pls)

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research/1_Experiment/Analysis")
dt <-read.delim("Area_harvest_spec.txt")

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



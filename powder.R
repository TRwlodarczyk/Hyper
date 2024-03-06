#Hyperspectral
#Powder analysis
#TOmasz Wlodarczyk

library(tidyverse)
library(ggpubr)
library(pls)
library(openxlsx)
library(stringr)


setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research/1_Experiment/Raw Data/pXRF")
dt <-read.delim("powder1.txt")


# Dead vs alive part of the same pot
ggplot(data=dt, aes(x=Wavelength)) +
  geom_line(aes(y=X12C_1), color="darkgreen") +
  geom_line(aes(y=X12C_2), color="darkgreen") +
  geom_line(aes(y=X12C_3), color="darkgreen") +
  geom_line(aes(y=X13C_1), color="darkred") +
  geom_line(aes(y=X13C_2), color="darkred") +
  geom_line(aes(y=X13C_3), color="darkred") +
  geom_line(aes(y=X14C_1), color="blue") +
  geom_line(aes(y=X14C_2), color="blue") +
  geom_line(aes(y=X14C_3), color="blue") +
  geom_line(aes(y=X1Zn2), color="orange") +
  geom_line(aes(y=X2Zn2), color="pink") +
  geom_line(aes(y=QA1), color="grey") +
  geom_line(aes(y=QA2), color="darkgrey") +
  geom_line(aes(y=QA3), color="black") +
  
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
  ggtitle("Green - center, Red - necrotic side7")

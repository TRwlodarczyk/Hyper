#plsw

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research/1_Experiment/Analysis")
dt <-read.delim("Plant_Parameters.txt")


library(ggplot2)
library(Rmisc)

dt[,15] <- sapply(dt[,15],as.numeric)

my_colors2 <- c( "#b3cde0","#6497b1", "#005b96", "#03396c", "#011f4b", "#ACDF87", "#68BB59", "#A4DE02", "#76BA1B", "#4C9A2A")


Roots_temporal <- summarySE(dt, measurevar="Root", groupvars=c("Pot_ID", "Measure"), na.rm=TRUE)
?summarySE

ggplot(Roots_temporal)+geom_line(aes(x = Measure, y = Root, group = Pot_ID))+
  geom_ribbon(aes(x=Measure, y=Root, ymin=Root-se, ymax=Root+se, group=Pot_ID, colour=factor(Pot_ID), fill=factor(Pot_ID)), alpha=0.2) + 
  ggtitle("Root growth in time") + xlab("Measurement")+ylab("Root length [cm]")+ scale_x_continuous(breaks=seq(1, 9, 1))+
  theme_minimal()+ scale_fill_manual(values=my_colors2)


str(dt)

C <- subset(dt, Treatment=="C")
Zn1 <- subset(dt, Treatment=="Zn1")
Zn2 <- subset(dt, Treatment=="Zn2")


  ggplot(Zn1, aes(x = Measure, y = Root, group = Pot_ID, color = Pot_ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "ddd",
       x = "Time",
       y = "NDVI") +
  scale_color_brewer(palette = "BrBG") 


  ggplot(Zn2, aes(x = Measure, y = Weight, group = Pot_ID, color = Pot_ID)) + 
    geom_point(size=1.5) +  # adds the scatter plot points
    geom_line(size=0.7) +   # connects the points with lines
    theme_minimal() + 
    labs(title = "Zn2",
         x = "Time",
         y = "Weight") +
    scale_color_brewer(palette = "BrBG") 
  
##
  
 
#Hyper
#New indicies


library(tidyverse)
library(viridis)

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research/1_Experiment/Analysis")
dt <-read.delim("Ahal_A_New_Indicies.txt")

dt <- subset(dt, Treatment!="avg")
C <- subset(dt, Treatment=="C")
Zn1 <- subset(dt, Treatment=="Zn1")
Zn2 <- subset(dt, Treatment=="Zn2")

T1 <- 

{
ggplot(dt, aes(x = Time, y = New1, group = ID, color = Treatment)) + 
  geom_point(size=1.5) +  # 
  geom_line(size=0.7) +   # 
  theme_minimal() + 
  labs(title = "Zinc All",
       x = "Time",
       y = "1880-1420/1880+1420") +
  scale_color_viridis_d() 

ggplot(dt, aes(x = Time, y = New2, group = ID, color = Treatment)) + 
  geom_point(size=1.5) +  # 
  geom_line(size=0.7) +   # 
  theme_minimal() + 
  labs(title = "Zinc All",
       x = "Time",
       y = "1880-1420/1880+1420") +
  scale_color_viridis_d() 


ggplot(dt, aes(x = Time, y = New3, group = ID, color = Treatment)) + 
  geom_point(size=1.5) +  # 
  geom_line(size=0.7) +   # 
  theme_minimal() + 
  labs(title = "Zinc All",
       x = "Time",
       y = "1880-1420/1880+1420") +
  scale_color_viridis_d() 


ggplot(dt, aes(x = Time, y = New4, group = ID, color = Treatment)) + 
  geom_point(size=1.5) +  # 
  geom_line(size=0.7) +   # 
  theme_minimal() + 
  labs(title = "Zinc All",
       x = "Time",
       y = "1880-1420/1880+1420") +
  scale_color_viridis_d() 




ggplot(dt, aes(x = Time, y = New5, group = ID, color = Treatment)) + 
  geom_point(size=1.5) +  # 
  geom_line(size=0.7) +   # 
  theme_minimal() + 
  labs(title = "Zinc All",
       x = "Time",
       y = "1880-1420/1880+1420") +
  scale_color_viridis_d() 



ggplot(dt, aes(x = Time, y = New6, group = ID, color = Treatment)) + 
  geom_point(size=1.5) +  # 
  geom_line(size=0.7) +   # 
  theme_minimal() + 
  labs(title = "Zinc All",
       x = "Time",
       y = "1880-1420/1880+1420") +
  scale_color_viridis_d() 


ggplot(dt, aes(x = Time, y = New7, group = ID, color = Treatment)) + 
  geom_point(size=1.5) +  # 
  geom_line(size=0.7) +   # 
  theme_minimal() + 
  labs(title = "Zinc All",
       x = "Time",
       y = "1880-1420/1880+1420") +
  scale_color_viridis_d() 



ggplot(dt, aes(x = Time, y = New8, group = ID, color = Treatment)) + 
  geom_point(size=1.5) +  # 
  geom_line(size=0.7) +   # 
  theme_minimal() + 
  labs(title = "Zinc All",
       x = "Time",
       y = "1880-1420/1880+1420") +
  scale_color_viridis_d() 



ggplot(dt, aes(x = Time, y = New9, group = ID, color = Treatment)) + 
  geom_point(size=1.5) +  # 
  geom_line(size=0.7) +   # 
  theme_minimal() + 
  labs(title = "Zinc All",
       x = "Time",
       y = "1880-1420/1880+1420") +
  scale_color_viridis_d() 


ggplot(dt, aes(x = Time, y = New10, group = ID, color = Treatment)) + 
  geom_point(size=1.5) +  # 
  geom_line(size=0.7) +   # 
  theme_minimal() + 
  labs(title = "Zinc All",
       x = "Time",
       y = "1880-1420/1880+1420") +
  scale_color_viridis_d() 


ggplot(dt, aes(x = Time, y = New11, group = ID, color = Treatment)) + 
  geom_point(size=1.5) +  # 
  geom_line(size=0.7) +   # 
  theme_minimal() + 
  labs(title = "Zinc All",
       x = "Time",
       y = "1880-1420/1880+1420") +
  scale_color_viridis_d() 


ggplot(dt, aes(x = Time, y = New12, group = ID, color = Treatment)) + 
  geom_point(size=1.5) +  # 
  geom_line(size=0.7) +   # 
  theme_minimal() + 
  labs(title = "Zinc All",
       x = "Time",
       y = "1880-1420/1880+1420") +
  scale_color_viridis_d() 



ggplot(dt, aes(x = Time, y = New13, group = ID, color = Treatment)) + 
  geom_point(size=1.5) +  # 
  geom_line(size=0.7) +   # 
  theme_minimal() + 
  labs(title = "Zinc All",
       x = "Time",
       y = "1880-1420/1880+1420") +
  scale_color_viridis_d() 

ggplot(dt, aes(x = Time, y = New14, group = ID, color = Treatment)) + 
  geom_point(size=1.5) +  # 
  geom_line(size=0.7) +   # 
  theme_minimal() + 
  labs(title = "Zinc All",
       x = "Time",
       y = "1880-1420/1880+1420") +
  scale_color_viridis_d() 


ggplot(dt, aes(x = Time, y = New15, group = ID, color = Treatment)) + 
  geom_point(size=1.5) +  # 
  geom_line(size=0.7) +   # 
  theme_minimal() + 
  labs(title = "Zinc All",
       x = "Time",
       y = "1880-1420/1880+1420") +
  scale_color_viridis_d() 

ggplot(dt, aes(x = Time, y = New16, group = ID, color = Treatment)) + 
  geom_point(size=1.5) +  # 
  geom_line(size=0.7) +   # 
  theme_minimal() + 
  labs(title = "Zinc All",
       x = "Time",
       y = "1880-1420/1880+1420") +
  scale_color_viridis_d() 

ggplot(dt, aes(x = Time, y = New17, group = ID, color = Treatment)) + 
  geom_point(size=1.5) +  # 
  geom_line(size=0.7) +   # 
  theme_minimal() + 
  labs(title = "Zinc All",
       x = "Time",
       y = "1880-1420/1880+1420") +
  scale_color_viridis_d() 

ggplot(dt, aes(x = Time, y = New18, group = ID, color = Treatment)) + 
  geom_point(size=1.5) +  # 
  geom_line(size=0.7) +   # 
  theme_minimal() + 
  labs(title = "Zinc All",
       x = "Time",
       y = "1880-1420/1880+1420") +
  scale_color_viridis_d() 
}
a <- ggplot(dt, aes(x = Time, y = New514minus2420, group = ID, color = Treatment)) + 
  geom_point(size=1.5) +  # 
  geom_line(size=0.7) +   # 
  theme_minimal() + 
  labs(title = "R-sq: 0.64",
       x = "Time",
       y = "514 - 2420") +
  scale_color_viridis_d() 



b <- ggplot(dt, aes(x = Time, y = New518by1420, group = ID, color = Treatment)) + 
  geom_point(size=1.5) +  # 
  geom_line(size=0.7) +   # 
  theme_minimal() + 
  labs(title = "R-sq: 0.621",
       x = "Time",
       y = "518/1420") +
  scale_color_viridis_d() 


ggplot(dt, aes(x = Time, y = newcombo1, group = ID, color = Treatment)) + 
  geom_point(size=1.5) +  # 
  geom_line(size=0.7) +   # 
  theme_minimal() + 
  labs(title = "Zinc All",
       x = "Time",
       y = "1880-1420/1880+1420") +
  scale_color_viridis_d() 

ggplot(dt, aes(x = Time, y = newcombo2, group = ID, color = Treatment)) + 
  geom_point(size=1.5) +  # 
  geom_line(size=0.7) +   # 
  theme_minimal() + 
  labs(title = "Zinc All",
       x = "Time",
       y = "1880-1420/1880+1420") +
  scale_color_viridis_d() 

ggplot(dt, aes(x = Time, y = newcombo3, group = ID, color = Treatment)) + 
  geom_point(size=1.5) +  # 
  geom_line(size=0.7) +   # 
  theme_minimal() + 
  labs(title = "Zinc All",
       x = "Time",
       y = "1880-1420/1880+1420") +
  scale_color_viridis_d() 

ggplot(dt, aes(x = Time, y = newcombo4, group = ID, color = Treatment)) + 
  geom_point(size=1.5) +  # 
  geom_line(size=0.7) +   # 
  theme_minimal() + 
  labs(title = "Zinc All",
       x = "Time",
       y = "1880-1420/1880+1420") +
  scale_color_viridis_d() 

c <- ggplot(dt, aes(x = Time, y = new19, group = ID, color = Treatment)) + 
  geom_point(size=1.5) +  # 
  geom_line(size=0.7) +   # 
  theme_minimal() + 
  labs(title = "R-sq: 0.705",
       x = "Time",
       y = "1410/1506") +
  scale_color_viridis_d() 



d <- ggplot(dt, aes(x = Time, y = new20, group = ID, color = Treatment)) + 
  geom_point(size=1.5) +  # 
  geom_line(size=0.7) +   # 
  theme_minimal() + 
  labs(title = "R-sq: 0.669",
       x = "Time",
       y = "707/2347") +
  scale_color_viridis_d() 

e <- ggplot(dt, aes(x = Time, y = new21, group = ID, color = Treatment)) + 
  geom_point(size=1.5) +  # 
  geom_line(size=0.7) +   # 
  theme_minimal() + 
  labs(title = "R-sq: 0.737",
       x = "Time",
       y = "1397 - 1539") +
  scale_color_viridis_d() 

f <- ggplot(dt, aes(x = Time, y = new22, group = ID, color = Treatment)) + 
  geom_point(size=1.5) +  # 
  geom_line(size=0.7) +   # 
  theme_minimal() + 
  labs(title = "R-sq: 0.667",
       x = "Time",
       y = "703 - 1417") +
  scale_color_viridis_d() 


library(ggpubr)

ggarrange(a, b, c, d, e, f, ncol = 3, nrow = 2, 
         common.legend = TRUE)#, #legend = "bottom")




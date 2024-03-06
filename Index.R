#Hyperspec
#Tomasz Wlodarczyk


library(tidyverse)
library(viridis)

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research/1_Experiment/Analysis")
#dt <-read.delim("Area_indicies.txt")
dt_av <-read.delim("Ahall_Aver_index.txt")


C <- subset(dt, Treatment=="C")
Zn1 <- subset(dt, Treatment=="Zn1")
Zn2 <- subset(dt, Treatment=="Zn2")
dt_av <- subset(dt, Treatment=="avg")
dt_av_Zn <- subset(dt_av, Treatment != "C")
#  scale_color_brewer(palette = "BrBG") 
#Index all samples
{
a <- ggplot(C, aes(x = Time, y = NDVI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 10",
       x = "Time",
       y = "NDVI") +
  scale_color_viridis_d() 


b <- ggplot(Zn1, aes(x = Time, y = NDVI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 500",
       x = "Time",
       y = "NDVI") +
  scale_color_viridis_d() 



c <- ggplot(Zn2, aes(x = Time, y = NDVI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 2000",
       x = "Time",
       y = "NDVI") +
  scale_color_viridis_d() 



d <- ggplot(C, aes(x = Time, y = RENDVI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 10",
       x = "Time",
       y = "RENDVI") +
  scale_color_viridis_d() 


e <- ggplot(Zn1, aes(x = Time, y = RENDVI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 500",
       x = "Time",
       y = "RENDVI") +
  scale_color_viridis_d() 



f <- ggplot(Zn2, aes(x = Time, y = RENDVI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 2000",
       x = "Time",
       y = "RENDVI") +
  scale_color_viridis_d() 


g <- ggplot(C, aes(x = Time, y = SRI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 10",
       x = "Time",
       y = "SRI") +
  scale_color_viridis_d() 


h <- ggplot(Zn1, aes(x = Time, y = SRI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 500",
       x = "Time",
       y = "SRI") +
  scale_color_viridis_d() 



i <- ggplot(Zn2, aes(x = Time, y = SRI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 2000",
       x = "Time",
       y = "SRI") +
  scale_color_viridis_d() 



j <- ggplot(C, aes(x = Time, y = PRI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 10",
       x = "Time",
       y = "PRI") +
  scale_color_viridis_d() 


k <- ggplot(Zn1, aes(x = Time, y = PRI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 500",
       x = "Time",
       y = "PRI") +
  scale_color_viridis_d() 



l <- ggplot(Zn2, aes(x = Time, y = PRI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 2000",
       x = "Time",
       y = "PRI") +
  scale_color_viridis_d() 


a1 <- ggplot(C, aes(x = Time, y = PSRI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 10",
       x = "Time",
       y = "PSRI") +
  scale_color_viridis_d() 

a2 <- ggplot(Zn1, aes(x = Time, y = PSRI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 500",
       x = "Time",
       y = "PSRI") +
  scale_color_viridis_d() 

a3 <- ggplot(Zn2, aes(x = Time, y = PSRI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 2000",
       x = "Time",
       y = "PSRI") +
  scale_color_viridis_d() 


m <- ggplot(C, aes(x = Time, y = NPQI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 10",
       x = "Time",
       y = "NPQI") +
  scale_color_viridis_d() 


n <- ggplot(Zn1, aes(x = Time, y = NPQI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 500",
       x = "Time",
       y = "NPQI") +
  scale_color_viridis_d() 



q <- ggplot(Zn2, aes(x = Time, y = NPQI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 2000",
       x = "Time",
       y = "NPQI") +
  scale_color_viridis_d() 



p <- ggplot(C, aes(x = Time, y = SIPI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 10",
       x = "Time",
       y = "SIPI") +
  scale_color_viridis_d() 


r <- ggplot(Zn1, aes(x = Time, y = SIPI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 500",
       x = "Time",
       y = "SIPI") +
  scale_color_viridis_d() 



s <- ggplot(Zn2, aes(x = Time, y = SIPI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 2000",
       x = "Time",
       y = "SIPI") +
  scale_color_viridis_d() 




t <- ggplot(C, aes(x = Time, y = LRDSI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 10",
       x = "Time",
       y = "LRDSI") +
  scale_color_viridis_d() 


w <- ggplot(Zn1, aes(x = Time, y = LRDSI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 500",
       x = "Time",
       y = "LRDSI") +
  scale_color_viridis_d() 



v <- ggplot(Zn2, aes(x = Time, y = LRDSI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 2000",
       x = "Time",
       y = "LRDSI") +
  scale_color_viridis_d() 


b1 <- ggplot(C, aes(x = Time, y = NDWI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 10",
       x = "Time",
       y = "NDWI") +
  scale_color_viridis_d() 

b2 <- ggplot(Zn1, aes(x = Time, y = NDWI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 500",
       x = "Time",
       y = "NDWI") +
  scale_color_viridis_d() 

b3 <- ggplot(Zn2, aes(x = Time, y = NDWI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 2000",
       x = "Time",
       y = "NDWI") +
  scale_color_viridis_d() 



c1 <- ggplot(C, aes(x = Time, y = GNDVI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 10",
       x = "Time",
       y = "GNDVI") +
  scale_color_viridis_d() 

c2 <- ggplot(Zn1, aes(x = Time, y = GNDVI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 500",
       x = "Time",
       y = "GNDVI") +
  scale_color_viridis_d() 


c3 <- ggplot(Zn2, aes(x = Time, y = GNDVI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 2000",
       x = "Time",
       y = "GNDVI") +
  scale_color_viridis_d() 




d1 <- ggplot(C, aes(x = Time, y = WI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 10",
       x = "Time",
       y = "WI") +
  scale_color_viridis_d() 

d2 <- ggplot(Zn1, aes(x = Time, y = WI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 500",
       x = "Time",
       y = "WI") +
  scale_color_viridis_d() 


d3 <- ggplot(Zn2, aes(x = Time, y = WI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "Zinc 2000",
       x = "Time",
       y = "WI") +
  scale_color_viridis_d() 



e1 <- ggplot(C, aes(x = Time, y = CARI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points CARIth lines
  theme_minimal() + 
  labs(title = "Zinc 10",
       x = "Time",
       y = "CARI") +
  scale_color_viridis_d() 

e2 <- ggplot(Zn1, aes(x = Time, y = CARI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points CARIth lines
  theme_minimal() + 
  labs(title = "Zinc 500",
       x = "Time",
       y = "CARI") +
  scale_color_viridis_d() 


e3 <- ggplot(Zn2, aes(x = Time, y = CARI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points CARIth lines
  theme_minimal() + 
  labs(title = "Zinc 2000",
       x = "Time",
       y = "CARI") +
  scale_color_viridis_d() 


f1 <- ggplot(C, aes(x = Time, y = ARI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points ARIth lines
  theme_minimal() + 
  labs(title = "Zinc 10",
       x = "Time",
       y = "ARI") +
  scale_color_viridis_d() 

f2 <- ggplot(Zn1, aes(x = Time, y = ARI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points ARIth lines
  theme_minimal() + 
  labs(title = "Zinc 500",
       x = "Time",
       y = "ARI") +
  scale_color_viridis_d() 


f3 <- ggplot(Zn2, aes(x = Time, y = ARI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points ARIth lines
  theme_minimal() + 
  labs(title = "Zinc 2000",
       x = "Time",
       y = "ARI") +
  scale_color_viridis_d() 



g1 <- ggplot(C, aes(x = Time, y = GRVI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points GRVIth lines
  theme_minimal() + 
  labs(title = "Zinc 10",
       x = "Time",
       y = "GRVI") +
  scale_color_viridis_d() 

g2 <- ggplot(Zn1, aes(x = Time, y = GRVI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points GRVIth lines
  theme_minimal() + 
  labs(title = "Zinc 500",
       x = "Time",
       y = "GRVI") +
  scale_color_viridis_d() 


g3 <- ggplot(Zn2, aes(x = Time, y = GRVI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points GRVIth lines
  theme_minimal() + 
  labs(title = "Zinc 2000",
       x = "Time",
       y = "GRVI") +
  scale_color_viridis_d() 



h1 <- ggplot(C, aes(x = Time, y = CAI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points CAIth lines
  theme_minimal() + 
  labs(title = "Zinc 10",
       x = "Time",
       y = "CAI") +
  scale_color_viridis_d() 

h2 <- ggplot(Zn1, aes(x = Time, y = CAI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points CAIth lines
  theme_minimal() + 
  labs(title = "Zinc 500",
       x = "Time",
       y = "CAI") +
  scale_color_viridis_d() 


h3 <- ggplot(Zn2, aes(x = Time, y = CAI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points CAIth lines
  theme_minimal() + 
  labs(title = "Zinc 2000",
       x = "Time",
       y = "CAI") +
  scale_color_viridis_d() 



i1 <- ggplot(C, aes(x = Time, y = LCI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points LCIth lines
  theme_minimal() + 
  labs(title = "Zinc 10",
       x = "Time",
       y = "LCI") +
  scale_color_viridis_d() 

i2 <- ggplot(Zn1, aes(x = Time, y = LCI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points LCIth lines
  theme_minimal() + 
  labs(title = "Zinc 500",
       x = "Time",
       y = "LCI") +
  scale_color_viridis_d() 


i3 <- ggplot(Zn2, aes(x = Time, y = LCI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points LCIth lines
  theme_minimal() + 
  labs(title = "Zinc 2000",
       x = "Time",
       y = "LCI") +
  scale_color_viridis_d() 


j1 <- ggplot(C, aes(x = Time, y = CVI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points CVIth lines
  theme_minimal() + 
  labs(title = "Zinc 10",
       x = "Time",
       y = "CVI") +
  scale_color_viridis_d() 

j2 <- ggplot(Zn1, aes(x = Time, y = CVI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points CVIth lines
  theme_minimal() + 
  labs(title = "Zinc 500",
       x = "Time",
       y = "CVI") +
  scale_color_viridis_d() 


j3 <- ggplot(Zn2, aes(x = Time, y = CVI, group = ID, color = ID)) + 
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points CVIth lines
  theme_minimal() + 
  labs(title = "Zinc 2000",
       x = "Time",
       y = "CVI") +
  scale_color_viridis_d() 



library(ggpubr)

ggarrange(a, d, g, j, a1, m, p, t, b1, c1, d1, e1, f1, 
          g1, h1, i1, j1, ncol = 4, nrow = 6, 
          common.legend = TRUE)#, #legend = "bottom")




ggarrange(b, e, h, k, a2, n, r, w, b2, c2, d2, e2, f2,
          g2, h2,i2,j2, ncol = 4, nrow = 6, 
          common.legend = TRUE)#, #legend = "bottom")



ggarrange(c, f, i, l, a3, q, s, v, b3, c3, d3, e3, 
          f3, g3, h3, i3, j3, ncol = 4, nrow = 6, 
          common.legend = TRUE)#, #legend = "bottom")

#ggarrange(a, b, c, d, e, f, g, h, i, j, k, l, a1, a2, 
#          a3, m, n, q, p, r, s, t, w, v, b1, b2, b3, 
#          c1, c2, c3, d1, d2, d3, e1, e2, e3, f1, f2,
#          f3, g1, g2, g3, h1, h2, h3, i1, i2, i3, j1, 
#          j2, j3, ncol = 5, nrow = 4, 
#          common.legend = TRUE)#, #legend = "bottom")


ggarrange(b, e, h, k,n,r,w, ncol = 3, nrow = 4, 
          common.legend = TRUE)#, #legend = "bottom")

ggarrange(a, d, g, j,m,p,t, ncol = 3, nrow = 4, 
          common.legend = TRUE)#, #legend = "bottom")


ggarrange(c, f, i, l,q,s,v, ncol = 3, nrow = 4, 
          common.legend = TRUE)#, #legend = "bottom")


}

#Index average

NDVI <- ggplot(dt_av, aes(x = Time, y = NDVI, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = NDVI - NDVI_sd, ymax = NDVI + NDVI_sd, fill = Treatment), alpha = 0.15, colour=NA, colour=NA) +
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "NDVI",
       x = "Time",
       y = "NDVI") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()

NDVI2 <- ggplot(dt_av_Zn, aes(x = Time, y = NDVI, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = NDVI - NDVI_sd, ymax = NDVI + NDVI_sd, fill = Treatment), alpha = 0.15, colour=NA, colour=NA) +
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "NDVI",
       x = "Time",
       y = "NDVI") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()


RENDVI <- ggplot(dt_av, aes(x = Time, y = RENDVI, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = RENDVI - RENDVI_sd, ymax = RENDVI + RENDVI_sd, fill = Treatment), alpha = 0.15, colour=NA) +
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "RENDVI",
       x = "Time",
       y = "RENDVI") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()

SRI <- ggplot(dt_av, aes(x = Time, y = SRI, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = SRI - SRI_sd, ymax = SRI + SRI_sd, fill = Treatment), alpha = 0.15, colour=NA) +
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "SRI",
       x = "Time",
       y = "SRI") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()


PRI <- ggplot(dt_av, aes(x = Time, y = PRI, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = PRI - PRI_sd, ymax = PRI + PRI_sd, fill = Treatment), alpha = 0.15, colour=NA) +
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "PRI",
       x = "Time",
       y = "PRI") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()

PSRI <- ggplot(dt_av, aes(x = Time, y = PSRI, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = PSRI - PSRI_sd, ymax = PSRI + PSRI_sd, fill = Treatment), alpha = 0.15, colour=NA) +
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "PSRI",
       x = "Time",
       y = "PSRI") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()

NPQI <- ggplot(dt_av, aes(x = Time, y = NPQI, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = NPQI - NPQI_sd, ymax = NPQI + NPQI_sd, fill = Treatment), alpha = 0.15, colour=NA) +
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "NPQI",
       x = "Time",
       y = "NPQI") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()

SIPI <- ggplot(dt_av, aes(x = Time, y = SIPI, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = SIPI - SIPI_sd, ymax = SIPI + SIPI_sd, fill = Treatment), alpha = 0.15, colour=NA) +
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "SIPI",
       x = "Time",
       y = "SIPI") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()

LRDSI <- ggplot(dt_av, aes(x = Time, y = LRDSI, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = LRDSI - LRDSI_sd, ymax = LRDSI + LRDSI_sd, fill = Treatment), alpha = 0.15, colour=NA) +
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "LRDSI",
       x = "Time",
       y = "LRDSI") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()

NDWI <- ggplot(dt_av, aes(x = Time, y = NDWI, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = NDWI - NDWI_sd, ymax = NDWI + NDWI_sd, fill = Treatment), alpha = 0.15, colour=NA) +
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "NDWI",
       x = "Time",
       y = "NDWI") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()

GNDVI <- ggplot(dt_av, aes(x = Time, y = GNDVI, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = GNDVI - GNDVI_sd, ymax = GNDVI + GNDVI_sd, fill = Treatment), alpha = 0.15, colour=NA) +
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "GNDVI",
       x = "Time",
       y = "GNDVI") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()

WI <- ggplot(dt_av, aes(x = Time, y = WI, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = WI - WI_sd, ymax = WI + WI_sd, fill = Treatment), alpha = 0.15, colour=NA) +
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "WI",
       x = "Time",
       y = "WI") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()

CARI <- ggplot(dt_av, aes(x = Time, y = CARI, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = CARI - CARI_sd, ymax = CARI + CARI_sd, fill = Treatment), alpha = 0.15, colour=NA) +
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "CARI",
       x = "Time",
       y = "CARI") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()

ARI <- ggplot(dt_av, aes(x = Time, y = ARI, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = ARI - ARI_sd, ymax = ARI + ARI_sd, fill = Treatment), alpha = 0.15, colour=NA) +
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "ARI",
       x = "Time",
       y = "ARI") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()

GRVI <- ggplot(dt_av, aes(x = Time, y = GRVI, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = GRVI - GRVI_sd, ymax = GRVI + GRVI_sd, fill = Treatment), alpha = 0.15, colour=NA) +
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "GRVI",
       x = "Time",
       y = "GRVI") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()

CAI <- ggplot(dt_av, aes(x = Time, y = CAI, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = CAI - CAI_sd, ymax = CAI + CAI_sd, fill = Treatment), alpha = 0.15, colour=NA) +
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "CAI",
       x = "Time",
       y = "CAI") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()

LCI <- ggplot(dt_av, aes(x = Time, y = LCI, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = LCI - LCI_sd, ymax = LCI + LCI_sd, fill = Treatment), alpha = 0.15, colour=NA) +
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "LCI",
       x = "Time",
       y = "RENDVI") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()

CVI <- ggplot(dt_av, aes(x = Time, y = CVI, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = CVI - CVI_sd, ymax = CVI + CVI_sd, fill = Treatment), alpha = 0.15, colour=NA, colour=NA) +
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "CVI",
       x = "Time",
       y = "CVI") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()

NPCI <- ggplot(dt_av, aes(x = Time, y = NPCI, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = NPCI - NPCI_sd, ymax = NPCI + NPCI_sd, fill = Treatment), alpha = 0.15, colour=NA, colour=NA) +
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "NPCI",
       x = "Time",
       y = "NPCI") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()

MCARI <- ggplot(dt_av, aes(x = Time, y = MCARI, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = MCARI - MCARI_sd, ymax = MCARI + MCARI_sd, fill = Treatment), alpha = 0.15, colour=NA, colour=NA) +
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "MCARI",
       x = "Time",
       y = "MCARI") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()

RVSI <- ggplot(dt_av, aes(x = Time, y = RVSI, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = RVSI - RVSI_sd, ymax = RVSI + RVSI_sd, fill = Treatment), alpha = 0.15, colour=NA, colour=NA) +
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "RVSI",
       x = "Time",
       y = "RVSI") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()

MTVI2 <- ggplot(dt_av, aes(x = Time, y = MTVI2, group = Treatment, color = Treatment)) + 
  geom_ribbon(aes(ymin = MTVI2 - MTVI2_sd, ymax = MTVI2 + MTVI2_sd, fill = Treatment), alpha = 0.15, colour=NA, colour=NA) +
  geom_point(size=1.5) +  # adds the scatter plot points
  geom_line(size=0.7) +   # connects the points with lines
  theme_minimal() + 
  labs(title = "MTVI2",
       x = "Time",
       y = "MTVI2") +
  scale_color_viridis_d() + # For discrete color scale based on the Treatment
  scale_fill_viridis_d()


library(ggpubr)

ggarrange(NDVI, RENDVI, SRI, PRI, PSRI, NPQI, SIPI, LRDSI,
          NDWI, GNDVI, WI, CARI, ARI, GRVI, CAI, LCI, CVI, NPCI, MCARI, RVSI, MTVI2, ncol = 4, nrow = 6, 
          common.legend = TRUE)

{

####
plot(dt$mining00000.asd~dt$Wavelength)


ggplot(data=dt, aes(x=Wavelength, y=mining00000.asd))+
  geom_line()


ggplot(data=dt, aes(x=Wavelength)) +
  geom_line(aes(y=mining00001.asd), color="blue") +
  geom_line(aes(y=mining00002.asd), color="orange") +  
  geom_line(aes(y=mining00005.asd), color="green") +
  geom_line(aes(y=mining00004.asd), color="purple")+
  geom_line(aes(y=mining00007.asd), color="red") +
  scale_x_continuous(limits = c(350, 2500), breaks = c(seq(350, 2500, by = 175),2500)) +
  scale_y_continuous(limits = c(0, 0.95), breaks = seq(0, 0.95, by = 0.05)) +
  geom_vline(xintercept = 700, linetype = "dashed", color = "#9a9a9a", size = 1.2) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
        axis.title.x = element_text(size = 19),
        axis.title.y = element_text(size = 19),
        axis.text.y = element_text(size=14),
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 13.5), 
        legend.title = element_text(size=15, face = "bold"))+
  guides(color = guide_legend(override.aes = list(size = 3.5)),
         shape = guide_legend(override.aes = list(size = 3.5))) +
  ylab("Reflectance")+
  xlab("Wavelength")



setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research")
dt <-read.delim("Correlation_table.txt")


# Assuming your dataset is named "dt"

# Create an empty vector to store the correlation coefficients
correlations <- vector("double", length = 2151)  # 2151 = 2155 - 5 + 1

# Loop through each column from 5 to 2155
for (i in 5:2155) {
  # Calculate the Spearman correlation between column i and "Zn_concentration"
  correlation <- cor(dt[, i], dt$Zn_concentration, method = "spearman")
  
  # Store the correlation coefficient in the vector
  correlations[i - 4] <- correlation
}

# Print the correlation coefficients
print(correlations)

# Assuming your dataset is named "dt"
library(openxlsx)

# Create an empty dataframe to store the correlation coefficients
cor_df <- data.frame(variable = character(2151), correlation = numeric(2151), stringsAsFactors = FALSE)

# Loop through each column from 5 to 2155
for (i in 5:2155) {
  # Calculate the Spearman correlation between column i and "Zn_concentration"
  correlation <- cor(dt[, i], dt$Zn_concentration, method = "spearman")
  
  # Store the variable name and correlation coefficient in the dataframe
  cor_df[i - 4, "variable"] <- names(dt)[i]
  cor_df[i - 4, "correlation"] <- correlation
}

# Save the dataframe as an Excel file
write.xlsx(cor_df, "correlations.xlsx", row.names = FALSE)

write.table(cor_df, file="C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research/corr.csv", sep=",", row.names = F)


cor.test(dt$Zn_concentration, dt$X1120, method="spearman")
cor.test(dt$Zn_concentration, dt$X686, method="spearman")
cor.test(dt$Zn_concentration, dt$X687, method="spearman")
cor.test(dt$Zn_concentration, dt$X1121, method="spearman")
cor.test(dt$Zn_concentration, dt$X1122, method="spearman")
cor.test(dt$Zn_concentration, dt$X684, method="spearman")
cor.test(dt$Zn_concentration, dt$X685, method="spearman")







corr <-read.delim("corr2.txt")

ggplot(data=corr, aes(x=variable, y=correlation)) +
  geom_line()





############# 1st derivative maxima


install.packages("pracma")  # For numerical differentiation

library(readxl)
library(pracma)
library(ggplot2)


setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research")
dt <-read.delim("Correlation_table.txt")



# Assuming reflectance data starts from column 5 and ends at column 2155
reflectance_data <- dt[, 5:2155]

# Calculate the first derivative of the reflectance data for each row
first_derivative <- apply(reflectance_data, 1, function(row) diff(row))

# Find the maxima in the first derivative (Red Edge position) for each row
maxima_indices <- sapply(seq_len(nrow(first_derivative)), function(i) {
  peaks <- pracma::findpeaks(first_derivative[i,])
  if (length(peaks) > 0) {
    peaks[which.max(peaks)]
  } else {
    NA
  }
})

# Get the corresponding wavelengths for the maxima
wavelengths <- seq(350, 2500, length.out = 2150)  # Keep the same length as maxima_indices

# Calculate the REP wavelengths for each sample based on the maxima indices
REP_wavelength <- wavelengths[maxima_indices]

# Create a data frame for plotting
plot_data <- data.frame(wavelength = wavelengths, first_derivative = first_derivative)

# Load the reshape2 package
library(reshape2)

# Convert first_derivative to a long format
plot_data_long <- melt(plot_data, id.vars = "wavelength", variable.name = "Sample", value.name = "FirstDerivative")

# Plot the first derivative
# Define a custom color palette with 12 distinct colors for each sample
custom_color_palette <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#a6cee3",
                          "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6", "#ffff99", "#b15928")

# Plot the first derivative with different colors for each sample
ggplot(plot_data_long, aes(x = wavelength, y = FirstDerivative, color = Sample)) +
  geom_line() +
  geom_vline(xintercept = REP_wavelength, linetype = "dashed", color = "red") +
  scale_color_manual(values = custom_color_palette) +  # Apply the custom color palette
  labs(x = "Wavelength (nm)", y = "First Derivative", title = "First Derivative of Reflectance Spectrum")
}


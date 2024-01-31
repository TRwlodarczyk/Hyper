library(ggplot2)
library(Rmisc)

setwd("C:/Users/Kamila/Desktop/PROJECT 5/MANUSCRIPT CARLOTA/Data files/Plant parameters")
dt.out <-read.delim("ALL_NO_OUTLIERS.txt")
dt.temporal <- read.delim("temporal_formated_no_out.txt")

my_colors2 <- c( "#b3cde0","#6497b1", "#005b96", "#03396c", "#011f4b", "#ACDF87", "#68BB59", "#A4DE02", "#76BA1B", "#4C9A2A")


Roots_temporal <- summarySE(dt.temporal, measurevar="RL1", groupvars=c("Soil", "Measurement"), na.rm=TRUE)
?summarySE

ggplot(Roots_temporal)+geom_line(aes(x = Measurement, y = RL1, group = Soil))+
  geom_ribbon(aes(x=Measurement, y=RL1, ymin=RL1-se, ymax=RL1+se, group=Soil, colour=factor(Soil), fill=factor(Soil)), alpha=0.2) + 
  ggtitle("Root growth in time") + xlab("Measurement")+ylab("Root length [cm]")+ scale_x_continuous(breaks=seq(1, 9, 1))+
  theme_minimal()+ scale_fill_manual(values=my_colors2)

ggplot(Roots_temporal) +
  geom_line(aes(x = Measurement, y = RL1, group = Soil)) +
  geom_ribbon(aes(x = Measurement, y = RL1, ymin = RL1 - se, ymax = RL1 + se, group = Soil, fill = factor(Soil)), 
              alpha = 0.6) + 
  scale_color_manual(values = my_colors2) +  # Set the border color to my_colors2
  ggtitle("Root growth in time") + 
  xlab("Measurement") + 
  ylab("Root length [cm]") + 
  scale_x_continuous(breaks = seq(1, 9, 1)) +
  theme_minimal() + 
  scale_fill_manual(values = my_colors2)

M1 <- subset(dt.temporal, Measurement =="1")
M2 <- subset(dt.temporal, Measurement =="2")
M3 <- subset(dt.temporal, Measurement =="3")
M4 <- subset(dt.temporal, Measurement =="4")
M5 <- subset(dt.temporal, Measurement =="5")
M6 <- subset(dt.temporal, Measurement =="6")
M7 <- subset(dt.temporal, Measurement =="7")
M8 <- subset(dt.temporal, Measurement =="8")
M9 <- subset(dt.temporal, Measurement =="9")

kk <- kruskal(M1$RL1, M1$Soil, group=TRUE,p.adj="bonferroni")
kk$groups
kk <- kruskal(M2$RL1, M2$Soil, group=TRUE,p.adj="bonferroni")
kk$groups
kk <- kruskal(M3$RL1, M3$Soil, group=TRUE,p.adj="bonferroni")
kk$groups
kk <- kruskal(M4$RL1, M4$Soil, group=TRUE,p.adj="bonferroni")
kk$groups
kk <- kruskal(M5$RL1, M5$Soil, group=TRUE,p.adj="bonferroni")
kk$groups
kk <- kruskal(M6$RL1, M6$Soil, group=TRUE,p.adj="bonferroni")
kk$groups
kk <- kruskal(M7$RL1, M7$Soil, group=TRUE,p.adj="bonferroni")
kk$groups
kk <- kruskal(M8$RL1, M8$Soil, group=TRUE,p.adj="bonferroni")
kk$groups
kk <- kruskal(M9$RL1, M9$Soil, group=TRUE,p.adj="bonferroni")
kk$groups

kk <- kruskal(M1$LN1, M1$Soil, group=TRUE,p.adj="bonferroni")
kk
kk <- kruskal(M2$LN1, M2$Soil, group=TRUE,p.adj="bonferroni")
kk$groups
kk <- kruskal(M3$LN1, M3$Soil, group=TRUE,p.adj="bonferroni")
kk$groups
kk <- kruskal(M4$LN1, M4$Soil, group=TRUE,p.adj="bonferroni")
kk$groups
kk <- kruskal(M5$LN1, M5$Soil, group=TRUE,p.adj="bonferroni")
kk$groups
kk <- kruskal(M6$LN1, M6$Soil, group=TRUE,p.adj="bonferroni")
kk$groups
kk <- kruskal(M7$LN1, M7$Soil, group=TRUE,p.adj="bonferroni")
kk$groups
kk <- kruskal(M8$LN1, M8$Soil, group=TRUE,p.adj="bonferroni")
kk$groups
kk <- kruskal(M9$LN1, M9$Soil, group=TRUE,p.adj="bonferroni")
kk$groups



ggplot(Nol_temporal)+geom_line(aes(x = Measurement, y = LN1, group = Soil))+
  geom_ribbon(aes(x=Measurement, y=LN1, ymin=LN1-se, ymax=LN1+se, group=Soil, colour=factor(Soil), fill=factor(Soil)), alpha=0.2) +
  ggtitle("Number of leaves in time") + xlab("Measurement")+ylab("Number of leaves")+ scale_x_continuous(breaks=seq(1, 9, 1)) +
  theme_minimal()

ggplot(Nol_temporal) +
  geom_line(aes(x = Measurement, y = LN1, group = Soil)) +
  geom_ribbon(aes(x = Measurement, y = LN1, ymin = LN1 - se, ymax = LN1 + se, group = Soil, fill = factor(Soil)), 
              alpha = 0.6) + 
  scale_color_manual(values = my_colors2) +  # Set the border color to my_colors2
  ggtitle("Number of leaves in time") + 
  xlab("Measurement") + 
  ylab("Number of leaves") + 
  scale_x_continuous(breaks = seq(1, 9, 1)) +
  theme_minimal() + 
  scale_fill_manual(values = my_colors2)




my_colors3 <- c( "#b3cde0","#6497b1", "#005b96", "#03396c", "#ACDF87", "#68BB59", "#A4DE02", "#76BA1B", "#4C9A2A")
biomass <- ggplot(dt.out, aes(x = Soil, y = Fresh_mass, fill= Soil)) +
  geom_boxplot()+ theme_bw() +ggtitle("Fresh biomass") + scale_fill_manual(values = my_colors3) + theme_minimal()
biomass
L9<-kruskal(dt.out$Fresh_mass, dt.out$Soil, group=TRUE,p.adj="bonferroni") 
L9$groups

leafarea <- ggplot(dt.out, aes(x = Soil, y = LA9, fill= Soil)) +
  geom_boxplot()+ theme_bw() +ggtitle("Leaf Area") + scale_fill_manual(values = my_colors3) + theme_minimal()
leafarea
L9<-kruskal(dt.out$LA9, dt.out$Soil, group=TRUE,p.adj="bonferroni") 
L9$groups

setwd("C:/Users/Kamila/Desktop/PROJECT 5/MANUSCRIPT CARLOTA/Data files/Plant parameters")
dt.DNA <-read.delim("DNA_biomass.txt")

DNA <- ggplot(dt.DNA, aes(x = Soil, y = DNA, fill= Soil)) +
  geom_boxplot()+ theme_bw() + scale_fill_manual(values = my_colors2) + theme_minimal()
DNA
L9<-kruskal(dt.DNA$DNA, dt.DNA$Soil, group=TRUE,p.adj="bonferroni") 
L9$groups

START <- subset(dt.PLS, Class=="START")
PLANT <- subset(dt.PLS, Class=="PLANT")
BLANK <- subset(dt.PLS, Class=="BLANK")


desired_order <- c("START", "BLANK", "PLANT")
# Convert the Class variable to a factor with the desired order
dt.PLS$Class <- factor(dt.PLS$Class, levels = desired_order)
library(ggplot2)
desired_order <- c("START", "BLANK", "PLANT")
# Convert the Class variable to a factor with the desired order
dt.DNA$Class <- factor(dt.DNA$Class, levels = desired_order)
DNA3 <- ggplot(dt.DNA, aes(x = Soil, y = DNA, fill= Class)) +
  geom_boxplot()+ theme_bw() + scale_fill_manual(values = c("grey","#BE6DB7", "#FDD36A")) + theme_minimal()
DNA3

NC <- subset(dt.PLS, Soil=="NC#1"|Soil=="NC#2"|Soil=="NC#3"|Soil=="NC#4"|Soil=="NC#5")
SC <- subset(dt.PLS, Soil=="SC#1"|Soil=="SC#2"|Soil=="SC#3"|Soil=="SC#4"|Soil=="SC#5")

DNA3NC <- ggplot(NC, aes(x = Soil, y = DNA, fill= Class)) +
  geom_boxplot()+ theme_bw() + scale_fill_manual(values = c("grey","#BE6DB7", "#FDD36A")) + theme_minimal() + ylim(0,2000)
DNA3NC
DNA3SC <- ggplot(SC, aes(x = Soil, y = DNA, fill= Class)) +
  geom_boxplot()+ theme_bw() + scale_fill_manual(values = c("grey","#BE6DB7", "#FDD36A")) + theme_minimal()
DNA3SC

DNA.S <- ggplot(START, aes(x = Soil, y = DNA, fill= Soil)) +
  geom_boxplot()+ theme_bw() + scale_fill_manual(values = my_colors2) + theme_minimal()
DNA.S
DNA.P <- ggplot(PLANT, aes(x = Soil, y = DNA, fill= Soil)) +
  geom_boxplot()+ theme_bw() + scale_fill_manual(values = my_colors3) + theme_minimal()
DNA.P
DNA.B <- ggplot(BLANK, aes(x = Soil, y = DNA, fill= Soil)) +
  geom_boxplot()+ theme_bw() + scale_fill_manual(values = my_colors2) + theme_minimal()
DNA.B



#### FOR SOIL

dt.soil <-read.delim("Carlota_soil.txt")
desired_order <- c("START", "BLANK", "PLANT")
# Convert the Class variable to a factor with the desired order
dt.soil$Class <- factor(dt.soil$Class, levels = desired_order)


Fe3NC <- ggplot(dt.soil, aes(x = Soil, y = Fe, fill= Class)) +
  geom_boxplot()+ theme_bw() + scale_fill_manual(values = c("grey","#BE6DB7", "#FDD36A")) + theme_minimal()
Fe3NC
Fe3SC <- ggplot(SC, aes(x = Soil, y = Fe, fill= Class)) +
  geom_boxplot()+ theme_bw() + scale_fill_manual(values = c("grey","#BE6DB7", "#FDD36A")) + theme_minimal()
Fe3SC

NO3N_3 <- ggplot(dt.soil, aes(x = Soil, y = NO3N, fill= Class)) +
  geom_boxplot()+ theme_bw() + scale_fill_manual(values = c("grey","#BE6DB7", "#FDD36A")) + theme_minimal()
NO3N_3
NH4N <- ggplot(dt.soil, aes(x = Soil, y = NH4N, fill= Class)) +
  geom_boxplot()+ theme_bw() + scale_fill_manual(values = c("grey","#BE6DB7", "#FDD36A")) + theme_minimal()
NH4N


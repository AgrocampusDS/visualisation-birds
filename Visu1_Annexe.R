############################################
### PACKAGES 
############################################

packages <- c("ape", "phytools", "readxl", "rgdal", "rgeos", "sf", "RColorBrewer", "viridis", "ggplot2", "gridExtra", "dplyr", "lme4", "cowplot",
              "lmerTest","scales","ggExtra","grid","rptR","MuMIn","nlme", "gridExtra", "tidyverse")
install.packages(setdiff(packages, rownames(installed.packages())))

library(ape)
library(phytools)
library(readxl)
require(rgdal)
library(rgeos)
library(sf)
library(RColorBrewer)
library(viridis)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(lme4)
library(cowplot)
library(lmerTest)
library(scales)
library(ggExtra)
library(grid)
library(rptR)
library(MuMIn)
library(nlme)
library(gridExtra)
library(tidyverse)

############################################
### IMPORTATION
############################################

# Traits par espèce BirdLife
dat <- readxl::read_xlsx("data/AVONET Supplementary dataset 1.xlsx", sheet = 2)
# Grille de Behrman
grid <- rgdal::readOGR("data/spatial/BehrmannMeterGrid_WGS84_land.shp")
# Polygones des pays du monde
countriesGeo <- rgdal::readOGR("data/spatial/all_countries.shp")
# Dictionnaire géographique Birdlife 
rangeData<-read.csv("data/spatial/AllSpeciesBirdLifeMaps2019.csv")

############################################
### PRE PROCESSING
############################################

# projection de Behrmann
P4S.Behr <- CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs")
gridB<-spTransform(grid,P4S.Behr)
countries <- spTransform(countriesGeo,P4S.Behr)
# simplification
countriesS <- gSimplify(countries, tol=10000,topologyPreserve=TRUE)
countriesS2 <- st_as_sf(countriesS)

# selection variables et nettoyage
dat <- dat %>% select(Species1, Primary.Lifestyle, Trophic.Niche, `Hand-Wing.Index`, Beak.Length_Culmen, Tarsus.Length, Mass) %>% 
  na.omit() %>% 
  mutate(Primary.Lifestyle = as.factor(Primary.Lifestyle), 
         Trophic.Niche = as.factor(Trophic.Niche), 
         # On récupère les résidus du modèle linéaire expliquant la longueur de la tarse puis du bec de l'oiseau parla masse de l'oiseau
         # -> on s'affranchit des différences par espèce et on garde la variabilité entre individus (comme une sorte de scale)
         tarsus.res = lm(log(dat$Tarsus.Length)~log(dat$Mass),data=dat)$resid, 
         bill.res = lm(log(dat$Beak.Length_Culmen)~log(dat$Mass),data=dat)$resid)


############################################
### CONSTRUCTION CARTES
############################################

# On associe à chaque espèce répertoriée dans le dico géographique les variables sélectionnées précédemment
rangeData <- rangeData %>% select(Species, WorldID) %>% 
  merge(y = dat[, c("Species1", "Hand-Wing.Index", "tarsus.res", "bill.res")], by.x = "Species", by.y = "Species1") %>% 
  rename(Hand.Wing.Index = `Hand-Wing.Index`) %>% 
  na.omit()

# On récupère les valeurs médianes des trois variables par pays (par ID)
med.C <- rangeData %>% group_by(WorldID) %>% 
  mutate(HWI_med = median(Hand.Wing.Index), 
         T_med = median(tarsus.res), 
         B_med = median(bill.res)) %>% 
  select(-Hand.Wing.Index, -tarsus.res, -bill.res, -Species) %>% 
  distinct()

# on implémente ces variables créées dans l'objet shapefile et on récupère uniquement les indicateurs qu'on a construit
gridB@data <- left_join(gridB@data, med.C)
gridB@data <- gridB@data %>% select(-count, -sum, -mean, -min, -max)

# On construit un gradient de couleurs pour ces trois indicateurs créés 
gridB@data <- gridB@data %>% 
  mutate(col.HWI_med = findInterval(HWI_med, quantile(HWI_med,probs=seq(0,1,0.02),na.rm=T), all.inside = TRUE), 
         col.T_med = findInterval(T_med, quantile(T_med,probs=seq(0,1,0.02),na.rm=T), all.inside = TRUE),
         col.B_med = findInterval(B_med, quantile(B_med,probs=seq(0,1,0.02),na.rm=T), all.inside = TRUE)) %>% 
  distinct()

gridB.sf <- st_as_sf(gridB)

# On créé des palettes de couleurs
colors<-rev(c(brewer.pal(9,"Spectral")))
colors<-colorRampPalette(colors)(50)


# Carte 
val_med <- gridB@data %>% select(HWI_med) %>% filter(HWI_med != "NA")
g1 <- ggplot(gridB.sf) +
  geom_sf(aes(fill = col.HWI_med)) +
  #scale_colour_gradientn(colours = colors) +
  scale_fill_gradientn(colours = colors, breaks = seq(0, max(val_med),length.out=6), name = "Taille de l'aile") +
  theme_void()+
  theme(legend.position="bottom", 
        legend.title = element_text(size=10,face="bold", vjust = 0.85))
  

g2 <- ggplot(gridB.sf) +
  geom_sf(aes(fill = col.T_med, color = col.T_med)) +
  scale_colour_gradientn(colors = colors) +
  scale_fill_gradientn(colors = colors) +
  theme_void()+
  theme(legend.position = "none")

g3 <- ggplot(gridB.sf) +
  geom_sf(aes(fill = col.B_med, color = col.B_med)) +
  scale_colour_gradientn(colors = colors) +
  scale_fill_gradientn(colors = colors) +
  theme_void()+
  theme(legend.position = "none")

############################################
### CONSTRUCTION BOXPLOTS
############################################

# On choisit de représenter les trois mêmes variables en fonction du mode de vie: 

# Aerial = en vol; 
# Terrestrial = à terre; 
# Insessorial = perché au dessus du sol; 
# Aquatic = sur l'eau, se nourrit dans l'eau;
# Generalist = pas de mode de vie particulier.

dat <- dat %>% mutate(Primary.Lifestyle = fct_recode(Primary.Lifestyle, "Aérien" = "Aerial"),
                      Primary.Lifestyle = fct_recode(Primary.Lifestyle, "Terrestre" = "Terrestrial"), 
                      Primary.Lifestyle = fct_recode(Primary.Lifestyle, "Insessoriel" = "Insessorial"),
                      Primary.Lifestyle = fct_recode(Primary.Lifestyle, "Aquatique" = "Aquatic"),
                      Primary.Lifestyle = fct_recode(Primary.Lifestyle, "Généraliste" = "Generalist"))

# On récupère une palette de couleurs issue des couleurs choisies pour les cartes 
MapColors<-colorRampPalette(colors)(101)

# Taille des ailes en fonction du style de vie
brks<-quantile(gridB@data$HWI_med,probs=seq(0,1,0.01),na.rm=T)
HWI_med.boxplot<-sapply(split(dat$`Hand-Wing.Index`,dat$Primary.Lifestyle),median,na.rm=T)
cols.hwi<-rep(NA,length(HWI_med.boxplot))
for(i in 1:length(HWI_med.boxplot)){
  diffVals<-abs(brks-HWI_med.boxplot[i])
  cols.hwi[i]<-MapColors[which.min(diffVals)]
}

b1 <- dat %>% ggplot(aes(x=Primary.Lifestyle,y=`Hand-Wing.Index`,fill=Primary.Lifestyle))+
  geom_boxplot()+
  theme_classic()+
  scale_fill_manual(values = cols.hwi)+
  theme(axis.text.x = element_text(angle= 45,hjust=1,color="black"))+
  labs(x=NULL, y ="Taille de l'aile")+
  theme(legend.position="none")

# Taille relative du tarse en fonction du style de vie
brks<-quantile(gridB@data$T_med,probs=seq(0,1,0.01),na.rm=T)
T_med.boxplot<-sapply(split(dat$tarsus.res, dat$Primary.Lifestyle),median,na.rm=T)
cols.T<-rep(NA,length(T_med.boxplot))
for(i in 1:length(T_med.boxplot)){
  diffVals<-abs(brks-T_med.boxplot[i])
  cols.T[i]<-MapColors[which.min(diffVals)]
}

b2 <- dat %>% ggplot(aes(x=Primary.Lifestyle,y=tarsus.res,fill=Primary.Lifestyle))+
  geom_boxplot()+
  theme_classic()+
  scale_fill_manual(values = cols.T) + 
  theme(axis.text.x = element_text(angle= 45,hjust=1,color="black"))+
  labs(x=NULL, y ="Taille relative du tarse")+
  theme(legend.position='none')

# Taille du bec en fonction de son régime alimentaire (mange à 60%) :

# Frugivore = fruits; 
# Granivore = graines et noix; 
# Nectarivore =  nectar; 
# Herbivore = matières végétales dans les systèmes non aquatiques; 
# Herbivore aquatic = matières végétales dans les systèmes aquatiques; 
# Invertivore = invertébrés des systèmes terrestres; 
# Vertivore =  vertébrés des systèmes terrestres; 
# Aquatic Predator = animaux vertébrés et invertébrés dans les systèmes aquatiques; 
# Scavenger = charognes, d'abats ou de déchets; 
# Omnivore = niches multiples, à l'intérieur ou à travers les niveaux trophiques, dans des proportions relativement égales. . 

dat.bis <- dat %>% mutate(Trophic.Niche = as.factor(Trophic.Niche),
                          Trophic.Niche = fct_recode(Trophic.Niche, "Herbivore aquatique" = "Herbivore aquatic"),
                          Trophic.Niche = fct_recode(Trophic.Niche, "Prédateur aquatique" = "Aquatic predator"), 
                          Trophic.Niche = fct_recode(Trophic.Niche, "Charognard" = "Scavenger")) %>% 
  filter(Trophic.Niche != "NA")

brks<-quantile(gridB@data$B_med,probs=seq(0,1,0.01),na.rm=T)
B_med.boxplot<-sapply(split(dat.bis$bill.res,dat.bis$Trophic.Niche),median,na.rm=T)
cols.b<-rep(NA,length(B_med.boxplot))
for(i in 1:length(B_med.boxplot)){
  diffVals<-abs(brks-B_med.boxplot[i])
  cols.b[i]<-MapColors[which.min(diffVals)]
}

b3 <- dat.bis %>% ggplot(aes(x=Trophic.Niche,y=bill.res,fill=Trophic.Niche))+
  geom_boxplot()+
  theme_classic()+
  scale_fill_manual(values = cols.b)+ 
  theme(axis.text.x = element_text(angle= 45,hjust=1,color="black"))+
  labs(x=NULL, y ="Taille relative du bec")+theme(legend.position='none')


grid.arrange(g1, b1, g2, b2, g3, b3, ncol=2, nrow = 3)


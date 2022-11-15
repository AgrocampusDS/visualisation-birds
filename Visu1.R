############################################
### PACKAGES 
############################################

packages <- c("ape", "phytools", "readxl", "rgdal", "rgeos", "sf", "RColorBrewer", "viridis", "ggplot2", "gridExtra", "dplyr", "lme4", "cowplot",
              "lmerTest","scales","ggExtra","grid","rptR","MuMIn","nlme")
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
### CONSTRUCTION CARTE
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
colors<-c(brewer.pal(9,"Blues")[2:4],brewer.pal(9,"YlGnBu")[5:9])
colors<-colorRampPalette(colors)(50)

# Carte 

ggplot(gridB.sf) +
  geom_sf(aes(fill = col.HWI_med, color = col.HWI_med)) +
  scale_colour_gradientn(colors = colors) +
  scale_fill_gradientn(colors = colors) +
  theme_void()

ggplot(gridB.sf) +
  geom_sf(aes(fill = col.T_med, color = col.T_med)) +
  scale_colour_gradientn(colors = colors) +
  scale_fill_gradientn(colors = colors) +
  theme_void()

ggplot(gridB.sf) +
  geom_sf(aes(fill = col.B_med, color = col.B_med)) +
  scale_colour_gradientn(colors = colors) +
  scale_fill_gradientn(colors = colors) +
  theme_void()




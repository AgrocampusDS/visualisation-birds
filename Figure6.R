####################################################################
# Figure 6 maps and boxplots of trait variation
####################################################################

####################################################################
# load data
####################################################################

# species level trait averages birdlife taxonomy
dat<-read.csv(paste0(TraitDataFolder,'AVONET1_BirdLife.csv'))
# Behrmann equal area (96 x 96km) grid shapefile
grid <- rgdal::readOGR(paste0(SpatialDataFolder,'BehrmannMeterGrid_WGS84_land.shp'))
# Country borders shapefile
countriesGeo <- rgdal::readOGR(paste0(SpatialDataFolder,'all_countries.shp'))
# gridded species geographic range data - Birdlife taxonomy 
rangeData<-read.csv(paste0(SpatialDataFolder,'AllSpeciesBirdLifeMaps2019.csv'))

####################################################################
# data processing and cleaning
####################################################################

# set grid and country shapefile to Behrmann projection
P4S.Behr <- CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs")
gridB<-spTransform(grid,P4S.Behr)
countries <- spTransform(countriesGeo,P4S.Behr)
# simplify country shapefile - needed for plotting   
countriesS <- gSimplify(countries, tol=10000,topologyPreserve=TRUE)
# convert to simple feature for plotting with ggplot
countriesS2 <- st_as_sf(countriesS)

# extract species names and the few traits needed here for plotting
dat<-dat[,match(c("Species1","Primary.Lifestyle","Trophic.Niche",
                  "Hand.Wing.Index","Beak.Length_Culmen","Tarsus.Length","Mass"),names(dat))]

# get rid of the few species without trophic niche data (newly described species)
dat<-na.omit(dat)

# reaoder factor levels
dat$Primary.Lifestyle <- factor(dat$Primary.Lifestyle,levels=c("Aerial","Aquatic","Insessorial","Terrestrial","Generalist"))
dat$Trophic.Niche <- factor(dat$Trophic.Niche,levels=c("Nectarivore","Herbivore aquatic","Frugivore","Granivore","Herbivore terrestrial","Aquatic predator","Invertivore","Vertivore","Scavenger","Omnivore"))

# calculate tarsus residuals from Body mass
dat$tarsus.res <- lm(log(dat$Tarsus.Length)~log(dat$Mass),data=dat)$resid
# calculate bill residuals from Body mass
dat$bill.res <- lm(log(dat$Beak.Length_Culmen)~log(dat$Mass),data=dat)$resid

cols <- c(brewer.pal(9,"Blues")[2:4],brewer.pal(9,"YlGnBu")[5:9])

######################################################################
# Maps
######################################################################

# assign trait data to each species in range database
rangeData$Hand.Wing.Index<-dat$Hand.Wing.Index[match(rangeData$Species,dat$Species1)]
rangeData$tarsus.res<-dat$tarsus.res[match(rangeData$Species,dat$Species1)]
rangeData$bill.res<-dat$bill.res[match(rangeData$Species,dat$Species1)]

# remove rows (i.e. cells x species) with no trait data
rangeData<-na.omit(rangeData)
length(unique(na.omit(rangeData$Species)))

# calculate median trait value per cell
HWIperCell<-split(rangeData$Hand.Wing.Index,rangeData$WorldID)
HWIperCell<-lapply(HWIperCell, function(x) x[!is.na(x)])
HWI_median_perCell<-sapply(HWIperCell,median)

TperCell<-split(rangeData$tarsus.res,rangeData$WorldID)
TperCell<-lapply(TperCell, function(x) x[!is.na(x)])
T_median_perCell<-sapply(TperCell,median)

BLperCell<-split(rangeData$bill.res,rangeData$WorldID)
BLperCell<-lapply(BLperCell, function(x) x[!is.na(x)])
BL_median_perCell<-sapply(BLperCell,median)

# assign values to grid shapefile
gridB@data$HWI_median_perCell<-NA
gridB@data$HWI_median_perCell[match(names(HWI_median_perCell),gridB@data$WorldID)]<-as.numeric(HWI_median_perCell)
gridB@data$T_median_perCell<-NA
gridB@data$T_median_perCell[match(names(T_median_perCell),gridB@data$WorldID)]<-as.numeric(T_median_perCell)
gridB@data$BL_median_perCell<-NA
gridB@data$BL_median_perCell[match(names(BL_median_perCell),gridB@data$WorldID)]<-as.numeric(BL_median_perCell)

# set scale and colors
brks <- quantile(gridB@data$HWI_median_perCell,probs=seq(0,1,0.02),na.rm=T)
gridB@data$col_HWI_median <- NA
gridB@data$col_HWI_median <- findInterval(gridB@data$HWI_median_perCell, brks, all.inside = TRUE)

brks <- quantile(gridB@data$T_median_perCell,probs=seq(0,1,0.02),na.rm=T)
gridB@data$col_T_median <- NA
gridB@data$col_T_median <- findInterval(gridB@data$T_median_perCell, brks, all.inside = TRUE)
max(diff(as.numeric(names(table(gridB@data$col_T_median)))))

brks <- quantile(gridB@data$BL_median_perCell,probs=seq(0,1,0.02),na.rm=T)
gridB@data$col_BL_median <- NA
gridB@data$col_BL_median <- findInterval(gridB@data$BL_median_perCell, brks, all.inside = TRUE)
max(diff(as.numeric(names(table(gridB@data$col_BL_median)))))

colors<-c(brewer.pal(9,"Blues")[2:4],brewer.pal(9,"YlGnBu")[5:9])
colors<-colorRampPalette(colors)(50)

# plot maps
gridB2 <- st_as_sf(gridB)

inches<-4.5
res<-600

# HWI

plot.f<-paste0(figFolder,'HWI_median_Map.tiff')
tiff(plot.f, width=inches*res, height=inches*res/2,units = "px")	
ggplot(gridB2) +
  geom_sf(aes(fill = col_HWI_median, color = col_HWI_median)) +
  scale_colour_gradientn(colors = colors) +
  scale_fill_gradientn(colors = colors) +
  theme_void()
dev.off()
system(paste0('open ',plot.f))

# tarsus

plot.f<-paste0(figFolder,'T_median_Map.tiff')
tiff(plot.f, width=inches*res, height=inches*res/2,units = "px")	
ggplot(gridB2) +
  geom_sf(aes(fill = col_T_median, color = col_T_median)) +
  scale_colour_gradientn(colors = colors) +
  scale_fill_gradientn(colors = colors) +
  theme_void()
dev.off()
system(paste0('open ',plot.f))

# beak length

plot.f<-paste0(figFolder,'BL_median_Map.tiff')
tiff(plot.f, width=inches*res, height=inches*res/2,units = "px")	
ggplot(gridB2) +
  geom_sf(aes(fill = col_BL_median, color = col_BL_median)) +
  scale_colour_gradientn(colors = colors) +
  scale_fill_gradientn(colors = colors) +
  theme_void()
dev.off()
system(paste0('open ',plot.f))

# make scale bar

Legend<-function(breaks = NULL, col = NULL, axis.args = NULL){
  ix <- 1:2
  iy <- breaks
  nBreaks <- length(breaks)
  midpoints <- (breaks[1:(nBreaks - 1)] + breaks[2:nBreaks])/2
  iz <- matrix(midpoints, nrow = 1, ncol = length(midpoints))
  image(iy, ix, t(iz), xaxt = "n", yaxt = "n", xlab = "",ylab = "", col = col, breaks = breaks)
  axis.args <- c(list(side = 1,padj=-1,mgp = c(3, 1, 0), las = 0),axis.args)
  do.call("axis", axis.args)
  box()
}

plot.f<-paste0(figFolder,'TraitMap_ScaleBar.tiff')
tiff(plot.f,width=1*res,h=0.1*res,units = "px")
par(mfrow=c(1,1))
par(mar=c(1,1,1,1))
brks <- seq(0,1,0.02)
Legend(breaks = seq(0,100,length.out=length(brks)), col = colors,
       axis.args = list(cex.axis=0.5,mgp=c(1,0,0),at=seq(0,100,25),labels=rep("",5),tck=0.2))
dev.off()
system(paste0('open ',plot.f))

# tick values
# HWI
round(as.numeric(quantile(gridB@data$HWI_median_perCell,probs=seq(0,1,0.01),na.rm=T)[seq(1,101,25)]),2)
# relative tarsus
round(as.numeric(quantile(gridB@data$T_median_perCell,probs=seq(0,1,0.01),na.rm=T)[seq(1,101,25)]),2)
# relative beak length
round(as.numeric(quantile(gridB@data$BL_median_perCell,probs=seq(0,1,0.01),na.rm=T)[seq(1,101,25)]),2)


######################################################################
# Boxplots 
######################################################################

#set the colors of the boxplots - use the median value to match with the color scale in the maps 

MapColors<-colorRampPalette(colors)(101)

# HWI colors primary lifestyle
HWIMapBreaks<-quantile(gridB@data$HWI_median_perCell,probs=seq(0,1,0.01),na.rm=T)
HWIBoxplotMedian<-sapply(split(dat$Hand.Wing.Index,dat$Primary.Lifestyle),median,na.rm=T)

cols.hwi<-rep(NA,length(HWIBoxplotMedian))
for(i in 1:length(HWIBoxplotMedian)){
  diffVals<-abs(HWIMapBreaks-HWIBoxplotMedian[i])
  cols.hwi[i]<-MapColors[which.min(diffVals)]
}

# Relative tarsus colors primary lifestyle
TMapBreaks<-quantile(gridB@data$T_median_perCell,probs=seq(0,1,0.01),na.rm=T)
TBoxplotMedian<-sapply(split(dat$tarsus.res,dat$Primary.Lifestyle),median,na.rm=T)

cols.tars<-rep(NA,length(TBoxplotMedian))
for(i in 1:length(TBoxplotMedian)){
  diffVals<-abs(TMapBreaks-TBoxplotMedian[i])
  cols.tars[i]<-MapColors[which.min(diffVals)]
}

# Relative beak length colors trophic niche
BLMapBreaks<-quantile(gridB@data$BL_median_perCell,probs=seq(0,1,0.01),na.rm=T)
BLBoxplotMedian<-sapply(split(dat$bill.res,dat$Trophic.Niche),median,na.rm=T)

cols.bill<-rep(NA,length(BLBoxplotMedian))
for(i in 1:length(BLBoxplotMedian)){
  diffVals<-abs(BLMapBreaks-BLBoxplotMedian[i])
  cols.bill[i]<-MapColors[which.min(diffVals)]
}

# HWI plot
p1 <- ggplot(data=dat,aes(x=Primary.Lifestyle,y=Hand.Wing.Index,fill=Primary.Lifestyle))+geom_boxplot()+theme_classic()+scale_fill_manual(values = cols.hwi)

# Tarsus plot: 
p2 <- ggplot(data=dat,aes(x=Primary.Lifestyle,y=tarsus.res,fill=Primary.Lifestyle))+geom_boxplot()+theme_classic()+scale_fill_manual(values = cols.tars)

# Beak plot:
p3 <- ggplot(data=dat,aes(x=Trophic.Niche,y=bill.res,fill=Trophic.Niche))+geom_boxplot()+theme_classic()+scale_fill_manual(values = cols.bill)

p1 <- p1 + theme(axis.text.x = element_text(angle= 45,hjust=1,color="black"))+
  labs(x=NULL, y ="Hand-wing index")+theme(legend.position='none')

p2 <- p2 + theme(axis.text.x = element_text(angle= 45,hjust=1,color="black"))+
  labs(x=NULL, y ="Relative tarsus length")+theme(legend.position='none')

p3 <- p3 + theme(axis.text.x = element_text(angle= 45,hjust=1,color="black"))+
  labs(x=NULL, y ="Relative beak length")+theme(legend.position='none')

plot.f<-paste0(figFolder,'Boxplots.pdf')
pdf(plot.f, width=4.7, height=2.35*3)	

# Arrange in grids.
grid.arrange(p1, p2, p3, ncol = 1)

dev.off()
system(paste0('open ',plot.f))

table(dat$Primary.Lifestyle) # Sample sizes lifestyle
table(dat$Trophic.Niche) # Sample sizes trophic niche 

library(sf)
library(tmap)
library(tmaptools)
library(maptools)
library(plyr)
library(tidyverse)
library(rgdal)
library(spatstat)
library(rgdal)
library(rgeos)
library(raster)
library(spdep)
library(classInt)
library(gstat)
library(spgwr)

##First, get the Futian  Boundaries

Futian <- st_read("FUTIAN.shp")

#plot it using the base plot function
qtm(Futian)
summary(Futian)

#transfor it to OGR and reproject it 
FutianOGR <- as(Futian, "Spatial")
wgs = "+init=epsg:4508"
FutianWGS <- spTransform(FutianOGR,wgs)

##Now get the location of all banks in the City
#transfor it to OGR and reproject it 
banks <- read_csv("bank2.csv")
banksSF <- st_as_sf(banks, coords =c("WGS84_LIN","WGS84_LAT"), crs = 4326)
banksOGR <- as(banksSF, "Spatial")
banksWGS <- spTransform(banksOGR,wgs)

#plot  banks in the city
tmap_mode("view")
tm_shape(FutianWGS) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(banksWGS) +
  tm_dots(col = "blue")


#now set a window as the borough boundary

window  <- as.owin(FutianWGS)
plot(window)

#create a ppp object
#KDE analysis 
x= as.numeric(banksWGS@coords[,c(1)])
y = as.numeric(banksWGS@coords[,c(2)])
banks.ppp <- ppp(x=x,y=y,window=window)
plot(banks.ppp,pch=16,cex=0.4, main="Destination points Of bank")
plot(density(banks.ppp, sigma = 500),main="KDE Plot of Destination points Of banks", palette="Blues")

#Add on Github.com

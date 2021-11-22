rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

## Make sure that the plot window is approximately square
## Note: the map files are large. It takes some time to draw the maps

## Optional: Setup independent square plot window in inches
#windows(width=8, height=8, record=TRUE)     

# setwd("E:\\Lectures2020\\QE20220\\TXShape")

library(TexMix)    ## For mapping functions
library(sp)
library(maptools)
##
## Read Poly Shapefiles (readShapePoly in library maptools)
##
# setwd('')
getinfo.shape("TXCnty2020/TXCnty.shp")


## rgdal::readOGR function to read shape files. Note: file extension *.shp no required
## the projection file *.prj is properly interpreted (here: long/lat coordinates)
## option  "integer64" required to import integer variables properly

## Get polygons of neighboring States
neig.shp <- rgdal::readOGR(dsn=getwd(), layer = "TXNeighbors", 
                           integer64 = "allow.loss", stringsAsFactors=TRUE)

# Get polygons of TX counties
county.shp <- rgdal::readOGR(dsn=getwd(), layer = "TXCnty", 
                             integer64 = "allow.loss", stringsAsFactors=TRUE)

## Get interstate layer
interState.shp <- rgdal::readOGR(dsn=getwd(), layer = "InterStateHwy", 
                                 integer64 = "allow.loss", stringsAsFactors=TRUE)
  
county.bbox <- bbox(county.shp)                             # county bounding box for map region
county.centroid <- coordinates(county.shp)                  # Get county centroids

summary(county.shp)

##
## Map categorical variable Urban/Rural counties
##
plot(neig.shp, axes=T, col=grey(0.9),                       # first background
     border="white", bg="lightblue",
     xlim=county.bbox[1,], ylim=county.bbox[2,])             
mapColorQual(county.shp$URBRURAL, county.shp,
             map.title="Urban/Rural TX Counties",
             legend.cex=0.8,
             legend.title = "Urban/Rural",add.to.map=T)
plot(interState.shp, col="tomato4", lwd=1, add=T)             # insert road network for orientation

##
## Color gradient map: unemployment rate with additional layers
##
plot(neig.shp, axes=T, col=grey(0.9),                         # first background (axes=T adds lat/long frame)
     border="white",  bg="lightblue",                         # border and background color 
     xlim=county.bbox[1,], ylim=county.bbox[2,])              # within bounding box

mapColorRamp(county.shp$UNEMPL, county.shp, breaks=8,         # second add map
             map.title="Proportion Unemployed Texans", 
             legend.cex=0.8,
             legend.title="Unemployment Rates",add.to.map=T)  # add.to.map=T over-plots provinces over neighbors

plot(interState.shp, col="tomato4", lwd=1, add=T)             # insert road network for orientation

##
## Plot bipolar map: variation around mean(unemployment rate)
##
vari.unempl <- county.shp$UNEMPL - mean(county.shp$UNEMPL)  # calculate variation of unemployment rates around its mean

hist(vari.unempl)                                           # explore distribution to
abline(v=0, lwd=3)
sum(vari.unempl >= 0)                                       # counties with above average unemployment
sum(vari.unempl < 0)                                        # counties with below average unemployment

plot(neig.shp, axes=T, col=grey(0.9),                       # first background
     border="white", bg="lightblue",
     xlim=county.bbox[1,], ylim=county.bbox[2,])             

##  Specification using: break.value=mean(county.shp$UNEMPL)
mapBiPolar(county.shp$UNEMPL, county.shp,                   # map variation of unemplopment rates
           neg.breaks=4, pos.breaks=4,  break.value=mean(county.shp$UNEMPL),
           map.title="Below and Above Mean Unemployment Rates", 
           legend.title="Unemployment Rate \n Variation",   # \n escape character for new line
           legend.cex=0.8, add.to.map=T)

plot(interState.shp, col="tomato4", lwd=1.5, add=T)         # insert road network for orientation


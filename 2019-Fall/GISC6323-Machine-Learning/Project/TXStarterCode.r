rm(list=ls(all=TRUE))                        # Start with clean workspace
setwd("E:\\Lectures2019\\EPPS6316\\Project\\TXShape")
library(DallasTracts)                        # For mapping and heteroscedastic regression models
library(spdep)                               # Spatial Analysis; also opens libraries "maptools" and "sp"
library(car)                                 # Basic regression diagnostics
##
## Read Poly Shapefiles (readShapePoly in library maptools)
##

## Get polygons of TX counties
county.shp <- rgdal::readOGR(dsn=getwd(), 
                     layer = "TXCnty", integer64 = "allow.loss")
## Get polygons of neighboring States
states.shp <- rgdal::readOGR(dsn=getwd(), 
                     layer = "TXNeighbors", integer64 = "allow.loss")
## Get lines interstate layer
Hwy.shp <- rgdal::readOGR(dsn=".", 
                  layer = "InterStateHwy", integer64 = "allow.loss")

summary(county.shp)                              # Check data in SpatialPolygonDataFrame

county.bbox <- bbox(county.shp)                  # Get Texas bounding box
county.centroid <- coordinates(county.shp)       # Get county centroids

##
## Qualitative Map of county urban status
##
plot(states.shp, xlim=county.bbox[1,], ylim=county.bbox[2,], col=grey(0.9))
mapColorQual(county.shp$URBRURAL, county.shp,     
             map.title="Urban and Rural Counties",
             legend.title="Urban versus\nRural",add.to.map=T)   
plot(Hwy.shp, col="salmon", lwd=2, add=T)

##
## Distance from Mexican border (Color Ramp)
##
plot(states.shp, xlim=county.bbox[1,], ylim=county.bbox[2,], col=grey(0.9))
mapColorRamp(county.shp$DISTMEX, county.shp, breaks=8,     
              map.title="Distance for the Mexican Border",
              legend.title="Distance",add.to.map=T)   
plot(Hwy.shp, col="salmon", lwd=2, add=T)

##
## Calculate some rates
##
trump <- county.shp$TRUMPVOT16/county.shp$TOTALVOT16*100
hist(trump)
clinton <- county.shp$CLINTONVOT/county.shp$TOTALVOT16*100
hist(clinton)

##
## Bipolar map of Trump votes centered around 50% (i.e. carrying the county)
##
plot(states.shp, xlim=county.bbox[1,], ylim=county.bbox[2,], col=grey(0.9))
mapBiPolar(trump, county.shp,                               
            neg.breaks=3, pos.breaks=8, break.value=50,  
            map.title="Counties with leading/lagging Trump Vote",
            legend.title="Below/Above 50%",add.to.map=T)   
plot(Hwy.shp, col="salmon", lwd=2, add=T)

##
## Colored scatterplot matrix
##
scatterplotMatrix(~clinton+HISPORG+MEDAGE+TURNOUT16, data=county.shp, 
                  smooth=list(col.smooth="orange", col.spread="orange"),
                  regLine = list(col="green") )

##
## Plot Spatial Links among the Counties
##
county.link <- poly2nb(county.shp, queen=F)                 # First get neighbors links
plot(county.shp,col=grey(0.7),border=grey(0.9), axes=T)     # Second plot areas
plot(county.link,coords=county.centroid, pch=19, cex=0.1,   # Third plot links focused at centroids
     col="blue", add=T)
title("Spatial Links among the TX Counties")                # forth add title
box()                                                       # fifth refresh the map frame

##
## Spatial Regression Residuals Autocorrelation Analysis
##
clinton.lm <- lm(log(clinton)~HISPORG+MEDAGE+TURNOUT16, data=county.shp)
summary(clinton.lm)
hist(residuals(clinton.lm))
##
## Bipolar map of Trump votes centered around 50% (i.e. carrying the county)
##
plot(states.shp, xlim=county.bbox[1,], ylim=county.bbox[2,], col=grey(0.9))
mapBiPolar(residuals(clinton.lm), county.shp,                               
           neg.breaks=5, pos.breaks=5, break.value=0,  
           map.title="Residual of Clinton Vote Model",
           legend.title="Resdiuals",add.to.map=T)   
plot(Hwy.shp, col="salmon", lwd=2, add=T)


county.linkW <- nb2listw(county.link, style="W")            # generated rowsum standardized link matrix
outlierSpatial <- moran.plot(residuals(clinton.lm),         # Moran plot with outlier diagnositics
                             county.linkW, labels=county.shp$NAME)          
lm.morantest(clinton.lm, county.linkW)                      # Test with W-coding scheme

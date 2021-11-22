rm(list=ls(all=TRUE))                        # Start with clean workspace
setwd("E:\\Lectures2019\\GISC6301\\Chapter14\\ItalyMaps")
library(DallasTracts)
library(spdep)                               # A library for spatial data analysis
library(maptools)                            # Library to read and map shape-files

##
## Read Poly Shapefiles (readShapePoly in library maptools)
##
getinfo.shape("Provinces.shp")

# data are in long/lat coordinates
neig.shp <- rgdal::readOGR("Neighbors.shp", GDAL1_integer64_policy=T)
prov.shp <- rgdal::readOGR("Provinces.shp", GDAL1_integer64_policy=T)
# neig.shp <- readShapePoly("Neighbors.shp",                # Get polygons of neighboring nations
#                           proj4string=CRS("+proj=longlat"))
# prov.shp <- readShapePoly("Provinces.shp",IDvar="PROVID", # Get polygons of 95 provinces
#                          proj4string=CRS("+proj=longlat"))
summary(prov.shp)
##
## Plot Fertility rate (Color Ramp)
##
prov.bbox <- bbox(prov.shp)                               # bounding box for map region
plot(neig.shp,axes=T,col=grey(0.9),border="white",        # first background (axes=T adds lat/long frame)
     xlim=prov.bbox[1,],ylim=prov.bbox[2,])               # within bounding box
mapColorRamp(prov.shp$TOTFERTRAT, prov.shp, breaks=8,     # second add map
              map.title="Spatial Pattern of Fertility Rate",
              legend.cex=0.8, legend.title="Fertility Rate",
              add.to.map=T)                                 # addToMap=T over-plots provinces over neighbors


##
## Plot Augmented Spatial Links among Italian Provinces
## Notes: Shape file has be edited so islands have a connection to mainland
## Alternatively use spdep::edit.nb function (does not work with RStudio)
##
prov.centroid <- coordinates(prov.shp)                           # Get province centroids
prov.link <- poly2nb(prov.shp, queen=F)                          # Generate neighbors links

plot(neig.shp,axes=T,col=grey(0.9),border="white",
     xlim=prov.bbox[1,],ylim=prov.bbox[2,])                      # First background
plot(prov.shp,col="palegreen3" ,border=grey(0.9), axes=T, add=T) # Second plot areas
plot(prov.link,coords=prov.centroid, pch=19, cex=0.1,            # Third plot links focused at centroids
     col="blue", add=T)
title("Augmented Spatial Links among Provinces")                 # Forth add title
box()                                                            # Fifth refresh frame

## generated row-sum standardized neighbors list => style option "W"
prov.linkW <- nb2listw(prov.link, style="W")

##
## Testing plain TOTFERRAT for spatial autocorrelation
##
fert.lm0 <- lm(TOTFERTRAT~1,data=prov.shp)                       # Regression with just the intercept
summary(fert.lm0)
mean(prov.shp$TOTFERTRAT)                                        # intercept identical to the mean
hist(residuals(fert.lm0),main="Variation around mean")

outlierSpatial <- moran.plot(residuals(fert.lm0),                # Moran plot with outlier diagnositics
                             prov.linkW, labels=prov.shp$PROVNAME,
                             main="Moran Plot of Italian Fertility Variations around the Mean")

## lm.moran wants a lm-model as input. Uses the residuals and independent variables for calculation
lm.morantest(fert.lm0, alternative="two.sided", prov.linkW)

##
## Regression models for fertility including model diagnostics
##
car::scatterplotMatrix(~TOTFERTRAT+ILLITERRAT+FEMMARAGE+DIVORCERAT+TELEPERFAM,
                       data=prov.shp)

fert.lm1 <- lm(TOTFERTRAT~FEMMARAGE, data=prov.shp)             # just one independent variable
summary(fert.lm1)
lm.morantest(fert.lm1, alternative="two.sided", prov.linkW)     # notice drop in spatial AC

## Full model
fert.lm2 <- lm(TOTFERTRAT~ILLITERRAT+FEMMARAGE+DIVORCERAT+TELEPERFAM, data=prov.shp)
summary(fert.lm2)
lm.morantest(fert.lm2, alternative="two.sided", prov.linkW)     # notice drop in spatial AC

## Perform Residual Inspection
car::qqPlot(fert.lm2, id.n=4, labels=prov.shp$PROVNAME)         # check distribution and outliers
car::residualPlots(fert.lm2, quadratic=F, type="rstudent")      # check for non-linearities
car::spreadLevelPlot(fert.lm2)                                  # check for heteroscedasticity

## Distribution of residuals
fert.resid <- residuals(fert.lm2)
hist(fert.resid,  freq=FALSE, main="Residuals of fert.lm2")
lines(density(fert.resid), col="red", lwd=2)

## K-S test to check whether the residuals follow a normal distribution. H0:normality
ks.test(fert.resid, pnorm, mean=mean(fert.resid), sd=sd(fert.resid))

##
## Map Regression Residuals (Bi-polar)
##
sum(fert.resid < 0)                         # identify number of pos/neg classes
sum(fert.resid >= 0)

plot(neig.shp,axes=T,col=grey(0.9),border="white",
     xlim=prov.bbox[1,],ylim=prov.bbox[2,])               # first background

mapBiPolar(fert.resid, prov.shp,                           # second regression residuals
           neg.breaks=5, pos.breaks=4, break.value=0.0,
           map.title="Fertility Model Residuals", legend.cex= 0.8,
           legend.title="Residuals",add.to.map=T)

##
## Spatial Residual Analysis
##
outlierSpatial <- moran.plot(fert.resid,        # Moran plot with outlier diagnositics
                             prov.linkW, labels=prov.shp$PROVNAME,
                             main="Moran Plot of Fertility Model Residuals")



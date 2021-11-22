library(spatstat)        # Key library for spatial point pattern analysis 
library(smacpod)         # Relative risk kernel densities based on statstat
library(maptools)        # To open shapefiles
rm(list=ls(all=TRUE))    # Clean objects from workspace
setwd("M:\\Lectures2017\\GISC7360\\Labs\\Lab02\\Patterns\\Shape")

## Importing shape-files
getinfo.shape("StudyArea.shp")
bnd.shp <- readShapePoly("StudyArea.shp")
getinfo.shape("RegClust.shp")
pts.shp <- readShapePoints("RegClust.shp")

## Converting shape-files to ppp objects
pts <- as.ppp(pts.shp)
pts$marks <- NULL
pts <- pts[as.owin(bnd.shp)]
summary(pts)
plot(pts, main="RegClust")

plot(Lest(pts))
plot(envelope(pts, fun=Lest), main="L-function")

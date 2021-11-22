library(spatstat)        # Key library for spatial point pattern analysis 
library(smacpod)         # Relative risk kernel densities based on statstat
library(maptools)        # To open shapefiles
rm(list=ls(all=TRUE))    # Clean objects from workspace

setwd("G:\\UTD_Classes\\2020Spring\\GISC7360_Pattern_Analysis\\Lab03\\SystematicPatterns")

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
axis(1);axis(2)

## standard L-function
plot(Lest(pts))
plot(envelope(pts, fun=Lest), main="L-function")

## centered L-function
Ls <- Lest(pts)
plot(Ls, . - r ~ r, main="Border Corrected L-r functions")
Lv <- envelope(pts, fun=Lest)
plot(Lv, .-r ~ r, main="L-r Envelope")


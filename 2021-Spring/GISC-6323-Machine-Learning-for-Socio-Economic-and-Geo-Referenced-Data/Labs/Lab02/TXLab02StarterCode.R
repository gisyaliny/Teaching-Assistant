rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

## Make sure that the plot window is approximately square
## Note: the map files are large. It takes some time to draw the maps

## Optional: Setup square plot window in inches independent of RStudio
# windows(width=8, height=8, record=TRUE)     

library(TexMix)    ## For mapping functions
library(sp)
library(maptools)
library(spdep)
##
## Read Poly Shapefiles (readShapePoly in library maptools)
##
getinfo.shape("TXCnty2020/TXCnty2020.shp")

## rgdal::readOGR function to read shape files. Note: file extension *.shp no required
## the projection file *.prj is properly interpreted (here: long/lat coordinates)
## option  "integer64" required to import integer variables properly

## Get polygons of neighboring States for geographic frame
## Use only for final maps because of slow drawing
neig.shp <- rgdal::readOGR(dsn='TXCnty2020', layer = "TXNeighbors",
                             integer64 = "allow.loss", stringsAsFactors=T)

## Get interstate layer for spatial orientation
interState.shp <- rgdal::readOGR(dsn='TXCnty2020', layer = "InterStateHwy",
                                 integer64 = "allow.loss", stringsAsFactors=T)

## Get polygons of TX counties
county.shp <- rgdal::readOGR(dsn='TXCnty2020', layer = "TXCnty2020",
                            integer64 = "allow.loss", stringsAsFactors=T)


  
county.bbox <- bbox(county.shp)                             # county bounding box for map region
county.centroid <- coordinates(county.shp)                  # Get county centroids

summary(county.shp)

## Convert religious adherence into the ordinal scale
county.shp$RELNUM <- unclass(county.shp$RELIGADHER)

##
## Map categorical variable Urban/Rural counties
##
plot(neig.shp, axes=T, col=grey(0.9),                       # first background (only for final maps)
     border="white", bg="lightblue",
     xlim=county.bbox[1,], ylim=county.bbox[2,])             
mapColorQual(county.shp$RELIGADHER, county.shp,
             map.title="Religious Adherence TX Counties",
             legend.cex=0.8,
             legend.title = "Adherence",add.to.map=T)
plot(interState.shp, col="tomato4", lwd=1, add=T)             # insert road network for orientation

##
## Color gradient map: population density with additional layers
##
plot(neig.shp, axes=T, col=grey(0.9),                         # first background (axes=T adds lat/long frame)
     border="white",  bg="lightblue",                         # border and background color 
     xlim=county.bbox[1,], ylim=county.bbox[2,])              # within bounding box

mapColorRamp(county.shp$MEDVALHOME, county.shp, breaks=8,     # second add map
             map.title="Median Home Values", 
             legend.cex=0.8,
             legend.title="Value",add.to.map=T)               # add.to.map=T over-plots provinces over neighbors

plot(interState.shp, col="tomato4", lwd=1, add=T)             # insert road network for orientation

##
## Plot bipolar map: variation around median(population change)
##

hist(county.shp$POPCHG)                                     # explore distribution to

plot(neig.shp, axes=T, col=grey(0.9),                       # first background
     border="white", bg="lightblue",
     xlim=county.bbox[1,], ylim=county.bbox[2,])             

mapBiPolar(county.shp$POPCHG, county.shp,                   # map variation of population change
           neg.breaks=2, pos.breaks=4,  break.value=median(county.shp$POPCHG),
           map.title="Below and Above Median Population Change", 
           legend.title="Population\nChange",   # \n escape character for new line
           legend.cex=0.8, add.to.map=T)

plot(interState.shp, col="tomato4", lwd=1.5, add=T)         # insert road network for orientation

##
## Get spatial structure distance matrices
##
nb <- spdep::poly2nb(county.shp, queen=F)                    # extract first order neighbors links
B <- spdep::nb2mat(nb, style="B")                            # convert neighbor list to binary matrix
plot(county.shp, col="palegreen3", border=grey(0.9), axes=T) # map topology
plot(nb, coords=coordinates(county.shp), pch=19, cex=0.1, col="blue", add=T)
title("Spatial Neighbors Links among TX County") 
topoDist <- 1-B; diag(topoDist) <- 0                         # convert into dissimilarity
topoDist <- as.dist(topoDist)                                # convert into distance object

##
## Get spherical distance matrix
##
sphDist <- sp::spDists(county.shp, longlat=T)                # spherical distance matrix among tracts in km
sphDist <- as.dist(sphDist)

##
## Get steps distance from topology
##
BNa <- ifelse(B==0,NA,B)                                     # recode 0's to NA's
allPath <- e1071::allShortestPaths(BNa)                      # calucate the shortest path among all nodes
topoDist <- as.dist(allPath$length)                          # number of steps from origin to destination node


library(ClustGeo);library(factoextra)

# get the data
Vars <- county.shp@data
summary(Vars)
varKeep <- c("OFFICERS","CRIMERATE","CLINTONVOT","TRUMPVOT16","AGE18PLUS","POP2010",
             "UNINSURED","MEDAGE","AGE18TO64","AGE18LESS","AGE65UP","PARTBLACK","PARTASIAN","SINGLEMOM","INCOME",
             "B2010PCT","B1990PCT","B1980PCT","B1960PCT","B1970PCT","B1950PCT","B1940PCT","POPDENSE")

xVars <- Vars[varKeep]
xVars <- as.data.frame(apply(xVars,2,as.numeric))
pc <-prcomp(xVars, scale = TRUE)
summary(pc)
fviz_eig(pc)
nofComp <- 8

summary(xVars)
featDist <- dist(pc$x[,1:nofComp])

##
## Evaluate mixture of feature and spatial dissimilarity.
##
K <- 12                            # Number of distinct clusters
range.alpha <- seq(0, 1, by=0.1)   # Evaluation range
cr <- choicealpha(featDist, sphDist, range.alpha, K, graph=TRUE)

tree <- hclustgeo(featDist, sphDist, alpha=0.2)
plot(tree, hang=-1)
rect.hclust(tree, k=K)

neighClus <- as.factor(cutree(tree, K))        # Determine cluster membership
table(neighClus)                               # number of tracts in each cluster


# Check the value of variables from each cluster
plotBoxesByFactor(xVars[,1:8], neighClus, ncol=2, zTrans=T, varwidth=F)
plotBoxesByFactor(xVars[,9:16], neighClus, ncol=2, zTrans=T, varwidth=F)
plotBoxesByFactor(xVars[,17:23], neighClus, ncol=2, zTrans=T, varwidth=F)
## Map Results

mapColorQual(neighClus, tractShp, 
             map.title ="Spatially constrained Cluster Analysis",
             legend.title="Cluster\nId.", legend.cex=0.9)

plot(lakesShp, col="skyblue", border="skyblue",add=T)
plot(hwyShp, col="cornsilk2", lwd=4, add=T)

plot(lakesShp, col="skyblue", border="skyblue",add=T)
plot(hwyShp, col="cornsilk2", lwd=4, add=T)

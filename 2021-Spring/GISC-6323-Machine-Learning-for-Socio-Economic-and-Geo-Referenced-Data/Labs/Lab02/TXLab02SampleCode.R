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
library(ClustGeo)

setwd("E:\\Lectures2021\\GISC6323\\Labs\\Lab02\\TXCnty2020")

## Get neighboring state as spatial reference frame
neig.shp <- rgdal::readOGR(dsn=".", layer = "TXNeighbors",
                             integer64 = "allow.loss", stringsAsFactors=T)

## Get interstate layer for spatial orientation
interState.shp <- rgdal::readOGR(dsn=".", layer = "InterStateHwy",
                                 integer64 = "allow.loss", stringsAsFactors=T)

## Get polygons of TX counties
county.shp <- rgdal::readOGR(dsn=".", layer = "TXCnty2020",
                            integer64 = "allow.loss", stringsAsFactors=T)

##
## Exclude Loving County
##
county.shp <- county.shp[county.shp$NAME!="Loving", ]


county.bbox <- bbox(county.shp)                             # county bounding box for map region
county.centroid <- coordinates(county.shp)                  # Get county centroids

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

# get the data
Vars <- county.shp@data
Vars <- transform(Vars,
  RELIGADHER = as.numeric(RELIGADHER),
  ROMVOT12 = ROMVOT12/TOTALVOT12,
  OBAVOT12 = OBAVOT12/TOTALVOT12,
  TRUMPVOT16 = TRUMPVOT16/TOTALVOT16,
  CLINTONVOT = CLINTONVOT/TOTALVOT16,
  DAYPOP18 = DAYPOP18 / NIGHTPOP18,
  POPDENSE = log(POPDENSE),
  BNEW = B2010PCT+B2000PCT,
  BOLD = B1960PCT+B1950PCT+B1940PCT+BPREPCT
)

varKeep <- c("CRIMERATE","CLINTONVOT","TRUMPVOT16","TURNOUT12","DISTMEX","MEDAGE",
             "RELIGADHER","UNINSURED","MEDAGE","AGE18LESS","AGE65UP","PARTBLACK",
             "PARTASIAN","HISPORG","SINGLEMOM","BIR15TO50","NOGOODENG","COLLEGEDEG",
             "UNEMPL","INCOME","POPDENSE","DAYPOP18","MEDVALHOME","BNEW","BOLD",
             "OWNOCCPCT","MOVEPREPCT","VMDDPC")

xVars <- Vars[varKeep]
row.names(xVars) <- 1:nrow(xVars)
featDist <- dist(scale(xVars))
##
## Evaluate mixture of feature and spatial dissimilarity.
##
K <- 12                            # Number of distinct clusters
range.alpha <- seq(0, 1, by=0.1)   # Evaluation range
cr <- choicealpha(featDist, sphDist, range.alpha, K, graph=TRUE)

tree <- hclustgeo(featDist, sphDist, alpha=0.3)
plot(tree, hang=-1)
rect.hclust(tree, k=K)

neighClus <- as.factor(cutree(tree, K))        # Determine cluster membership
table(neighClus)                               # number of tracts in each cluster


# Check the value of variables from each cluster
plotBoxesByFactor(xVars[,1:7], neighClus, ncol=2, zTrans=T, varwidth=F)
plotBoxesByFactor(xVars[,8:14], neighClus, ncol=2, zTrans=T, varwidth=F)
plotBoxesByFactor(xVars[,15:21], neighClus, ncol=2, zTrans=T, varwidth=F)
plotBoxesByFactor(xVars[,22:28], neighClus, ncol=2, zTrans=T, varwidth=F)

## Map Results
plot(neig.shp, axes=T, col=grey(0.9),                       # first background (only for final maps)
     border="white", bg="lightblue",
     xlim=county.bbox[1,], ylim=county.bbox[2,])        

mapColorQual(neighClus, county.shp, 
             map.title ="Spatially constrained Cluster Analysis",
             legend.title="Cluster\nId.", legend.cex=0.9, add=T)

plot(interState.shp, col="tomato4", lwd=1, add=T)             # insert road network for orientation

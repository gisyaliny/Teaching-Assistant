rm(list=ls(all=T))
library(maptools);library(car);library(spatial);library(RColorBrewer); library(classInt)
setwd("G:\\UTD_Classes\\2020Spring\\GISC7360_Pattern_Analysis\\Lecture05\\KansasDEM")              # Directory with data files

windows(width=12, height=9, record=FALSE, buffered=FALSE)

identifyPts <- function(pts.grid, nOfSel=1, pch=18, col="red", ...)
## Manually identifies selection points, and plots a symbol as they are selected
{
    coord <- coordinates(pts.grid)
    selIdx <- rep(FALSE, nrow(coord))                             # Stores selected pts by TRUE
    pntCount <- 0
    while(pntCount < nOfSel) {
        selpt <- identify(coord[!selIdx,], n=1, plot=FALSE, 
                          atpen=TRUE, offset=0.0, tolerance=0.05) # just allow for unselected points
        plot(pts.grid[selpt,], pch = pch, col=col, add=TRUE)
        selIdx[selpt] <- TRUE                                     # set selected point to TRUE
        pntCount <- pntCount + 1
        cat(pntCount," ")
    }
    cat("\n")
    return(selIdx)
}  #end::identifyPts

gridDEM <- rgdal::readOGR(dsn=getwd(), layer="DEMPointsDisRiv", integer64="warn.loss")
proj4string(gridDEM)
grid.data <- as.data.frame(gridDEM)
boxDEM <- bbox(gridDEM)
cat("Latitude range: ",boxDEM[1,2] - boxDEM[1,1],"\n")
cat("Longitude range: ",boxDEM[2,2] - boxDEM[2,1],"\n")
summary(grid.data)

river <- rgdal::readOGR(dsn=getwd(), layer="RiversRevised", integer64="warn.loss")
proj4string(river)
## plot DEM
breakpts <- seq(250,1000,by=25)                                                 # Define Elevation classes
hist(grid.data$ELEVATION, breaks=breakpts,
     xlab="Elevation (meters)", main="Distribution of DEM Elevations")                                     # Check Distribution
ncl <- length(breakpts)-1                                                       # Number of classes
pal <- terrain.colors(ncl)                                                      # get colors
cols <- pal[findInterval(grid.data$ELEVATION,breakpts,rightmost.closed=T)]      # assign appropriate color to each residual
plot(gridDEM, axes=T, col=cols, pch=15,cex=1)                                   # Observed DEM Grid
plot(river,col="blue",add=T)
title("Sample DEM Locations")

##
## Manually sample n of pts
##  
#selPts <- sample(1:nrow(gridDEM), 100, replace=FALSE)                          # Random sampling without replacement and equal prob
selPts <- identifyPts(gridDEM, 100, pch=18)                                     # Sample interactively nofsample points
sampleDEM <- gridDEM[selPts,]                                                   # Obtain subset of sampled point locations

## Quality check
elevHist <- Hmisc::histbackback(sampleDEM$ELEVATION, gridDEM$ELEVATION, 
                                prob=TRUE, xlab=c("sample","map"),
                                main="Elevation")
barplot(-elevHist$left, col="dark grey", horiz=TRUE, space=0, add=TRUE, axes=FALSE)
barplot(elevHist$right, col="grey", horiz=TRUE, space=0, add=TRUE, axes=FALSE)

## KS-test
ks.test(elevHist$left,elevHist$right)

## Write shape-file to working directory
rgdal::writeOGR(sampleDEM, dsn=getwd(), layer="sampleDEM",
                overwrite_layer=T, driver="ESRI Shapefile")

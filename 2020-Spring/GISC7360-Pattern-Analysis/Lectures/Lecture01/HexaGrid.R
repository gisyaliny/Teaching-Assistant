library(deldir)
setwd("E:\\Lectures2020\\GISC7360\\Lecture01")
rm(list=ls())                      # Clean objects from workspace

################################################################################
## Objective: Generate a set of points on a triangular grid                   ##
##            The narrowing distances at higher latitudes are accommodated    ##
##            The grid can be used to generate hexagonal tiles with           ##
##            Voronoi polygons whose area is constant                         ##
##            Note: lat/long decimal degree coordinates are required          ##
################################################################################

xIncrement <- 2.0                   # triangle edge length in decimal degrees
xRange <- c(-120,-72)               # West-East extend of US in decimal degrees
yRange <- c(23,51)                  # South-North extend of US in decimal degrees

xMidLine <- (xRange[2]+xRange[1])/2   # South-North center axis

yIncrement <- sqrt(xIncrement^2-(xIncrement/2)^2)  # Latitude offset: equivalent to cos(pi/6)
xShift <- xIncrement/4                             # even-odd row offset

xSeq <- seq(xRange[1],xRange[2],by=xIncrement) 
ySeq <- seq(yRange[1],yRange[2],by=yIncrement)

hexaXY <- expand.grid(lon=xSeq,lat=ySeq)

yDim <- length(ySeq)
xDim <- length(xSeq)
for(j in 1:yDim) {                                          # cycle over rows
  xShift <- -xShift                                         # switch offset for odd/even rows
  for (i in 1:xDim) {
    hexaXY[i + xDim*(j-1), 1] <- (hexaXY[i + xDim*(j-1), 1] + xShift)     # shift rows
    hexaXY[i + xDim*(j-1), 1] <- hexaXY[i + xDim*(j-1), 1] - xMidLine     # center around mid-axis
    hexaXY[i + xDim*(j-1), 1] <- hexaXY[i + xDim*(j-1), 1]  /             # scale by cos(latitude)
                                 cos(hexaXY[i + xDim*(j-1), 2]/180*pi)
    hexaXY[i + xDim*(j-1), 1] <- hexaXY[i + xDim*(j-1), 1] + xMidLine     # reverse centering
  }
}    
hexaTile <- deldir(hexaXY$lon,hexaXY$lat)                   # Voronoi diagram
plot(tile.list(hexaTile),asp=1, pch=".",                    # plot tessellation
     ylim=yRange, xlim=xRange,
     xlab="Longitude", ylab="Latitude",
     main="Hexagonal Grid over Conterminous US")                    
abline(v=xMidLine, col="red")                               # project reference
#foreign::write.dbf(hexaXY,"USAHexaGridPts.dbf")            # save hexagonal grid for GIS

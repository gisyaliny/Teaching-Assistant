library(sp)
rm(list=ls())               # Clean objects from workspace

##
## Read shape-file using maptools (old approach)
##
#Shape <- maptools::readShapePoly("E:\\Lectures2020\\GISC7360\\Lecture01\\Region.shp")
##
## Read shape-file using rgdal (new approach)
## Notes: [a] It uses the "*.prj" information; [b] Layers should not have the
##        extension "*.sph"; [c] long integers are converted by default to factors,
##        to avoid this set option integer64="warn.loss"
##
Shape <- rgdal::readOGR(dsn="G:\\UTD_Classes\\2020Spring\\GISC7360_Pattern_Analysis\\Lecture01\\RegionCityShape", 
                        layer="Region", integer64="warn.loss")

## Plot Polygons
polyCol <- c("orange1","orange2","orange3","orange4")
plot(Shape, border="white", lwd=2, col=polyCol, axes=T)
box(lwd=2)
title("Problematic Shape Points and Area Layouts")
points(coordinates(Shape),col="red",pch=7, cex=1.5)       # centroids (center of gravity) as the are stored in shape file
                                                          # calculated by area-weighted triangle centroids

regionSet <- unique(Shape$REGION)                         # get region IDs
cent.pts <- matrix(NA,nrow=length(regionSet), ncol=2)     # initialize arithmetic centroid based on shape points
for (i in regionSet) {
  for (j in 1:length(Shape@polygons[[i]]@Polygons)){
    points(Shape@polygons[[i]]@Polygons[[j]]@coords,pch=19,cex=0.7)     # map the shape points
    if(j == 1) polyCoords <- Shape@polygons[[i]]@Polygons[[j]]@coords   # collect coordinates
    else polyCoords <- rbind(polyCoords,Shape@polygons[[i]]@Polygons[[j]]@coords)
  }
  cent.pts[i,] <- colMeans(polyCoords)                   # calculate centroids from shape points
  }

points(cent.pts,pch=10,cex=1.5,col="blue")               # and map the calculated centroids

legend(locator(1),                                       # interactive legend placement
       legend=c("Weighted Triangle Centroid","Shape-Point Centroid"),
       col=c("red","blue"),
       pch=c(7,10))

## Analyses Structure
Shape
str(Shape)
getClass("SpatialPolygonsDataFrame")
# getClass("Polygons")
# getClass("Polygon")
# getClass("Line")
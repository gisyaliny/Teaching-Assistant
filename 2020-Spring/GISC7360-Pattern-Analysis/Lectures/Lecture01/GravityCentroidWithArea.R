library(sp)
rm(list=ls())               # Clean objects from workspace
##
## Calculates: [a] Center of Gravity (triangle area weighted triangle centers)
##             [b] Area of polygon (may have negative sign)
## see algorithm at https://en.wikipedia.org/wiki/Polygon#Area_and_centroid
##

## Read shape-file
Shape <- rgdal::readOGR(dsn="G:\\UTD_Classes\\2020Spring\\GISC7360_Pattern_Analysis\\Lecture01\\BowTiePolyShape", 
                        layer="BowTiePolyShape", integer64="warn.loss")

coord <- Shape@polygons[[1]]@Polygons[[1]]@coords         # extracted shape points of closed polygon
#coord <- slot(slot(slot(Shape,"polygons")[[1]],"Polygons")[[1]],"coords")
#Shape@polygons[[1]]@area
#Shape@polygons[[1]]@labpt

plot(Shape, axes=T, asp=1)
points(coord,col="orange",pch=20, cex=2)
points(coordinates(Shape),col="red",pch=1, cex=1.5) # red circle 
box()

npts <-nrow(coord)
coord[1,] == coord[npts, ]                         # are start- and end-point coordinates identical?
coord <- coord[npts:1, ]                           # reorder the coordinate sequence

subCentArea <- matrix(NA, nrow = npts-1, ncol=3)   # 1:=xCentroid, 2:=yCentroid, 3:=Area
for (i in 1:(npts-1)){
  subCentArea[i,1] <- coord[i,1] + coord[i+1,1]
  subCentArea[i,2] <- coord[i,2] + coord[i+1,2]
  subCentArea[i,3] <- (coord[i,1]*coord[i+1,2] - coord[i+1,1]*coord[i,2])
} # end::for

## Area: abs(Area) == Shape@polygons[[1]]@area  
A <- sum(subCentArea[,3])/2                        # depending on direction may have wrong sign
cat("Area:", A)
## Center of Gravity: identical to Shape@polygons[[1]]@labpt
cX <- sum(subCentArea[,1]*subCentArea[,3])/(6*A)
cY <- sum(subCentArea[,2]*subCentArea[,3])/(6*A)
points(cbind(cX,cY), col="blue", pch=3, cex=2)            # blue cross



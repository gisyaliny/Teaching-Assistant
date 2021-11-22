library(sp); library(deldir); library(plyr)
rm(list=ls())

## Read shape-file
Shape <- rgdal::readOGR(dsn="E:\\Lectures2020\\GISC7360\\Lecture01", 
                        layer="BowTiePolyShape", integer64="warn.loss")

coord <- Shape@polygons[[1]]@Polygons[[1]]@coords         # extracted shape points
#coord <- slot(slot(slot(Shape,"polygons")[[1]],"Polygons")[[1]],"coords")
#coord <- coord[-1,]                                      # remove repeated start point

plot(Shape, axes=T, asp=1)
points(coord,col="orange",pch=20, cex=2)
box()

## Generate a Delaunay triangulation
voro.tri <- deldir(coord[,1],coord[,2])
voro.tile <- tile.list(voro.tri)
plot(voro.tile,close=FALSE, add=T)

## extract tile vertices into matrix
voro.vert <- plyr::ldply(voro.tile,function(coord) cbind(coord$x,coord$y))

## Remove dublicated vertices
voro.vert <- unique(voro.vert, MARGIN=1)

## Select vertices inside the polygon
idx <- point.in.polygon(voro.vert[,1],voro.vert[,2], coord[,1],coord[,2])
voro.vert <- voro.vert[idx==1,]

## "Minimization" part of optimization. 
## Distance to closest shape-point
min.dist <- apply(voro.vert, 1,
                  function(x) min(spDistsN1(coord, x)) )

## "Maximization" part of optimization with which.max function
## Vertex furthest to it's closest shape point
points(voro.vert[which.max(min.dist), ], pch=16, col="red", cex=2)


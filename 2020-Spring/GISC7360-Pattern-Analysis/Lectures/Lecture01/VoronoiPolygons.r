library(tripack)
rm(list=ls())               # Clean objects from workspace
##
## Voronoi polygon (aka Thiessen polygon), Delaunay triangulation, and convex hull
##

## Generate 10 points in a 10 by 10 square
nPoints <- 10
x <- runif(nPoints,min=0,max=10)
y <- runif(nPoints,min=0,max=10)
plot(x, y, xlim=c(0,10), ylim=c(0,10), pch=16, cex=0.7, col="red", asp=1)

## Generate Voronoi polygons
p.mos <- voronoi.mosaic(x, y)
plot(p.mos,col="red",do.points=T,add=T)

## Generate a Delaunay triangulation
p.mesh <- tri.mesh(x, y)
plot(p.mesh, lty=1, add=T)

## Generate the convex hull
convex.hull(p.mesh, plot.it=T,col="blue",lwd=2, add=T)

##
## See also the simpler library "deldir"
##
library(deldir)
d <- deldir(x, y)
l <- tile.list(d)
g <- tile.centroids(l)

plot(l, close=FALSE, col.pts="red", xlim=c(0,10), ylim=c(0,10), cex=1, asp=1)
points(g, pch=3, cex=1.5, col="red")


rm(list=ls())
library(spatstat); library(maptools); library(sp)

## Read network shape-file
streets <- rgdal::readOGR(dsn="E:\\Lectures2020\\GISC7360\\Lecture03\\Soho", 
                        layer="SohoStreets", integer64="warn.loss")
proj4string(streets)                              # Current map projection
plot(streets, col="salmon", lwd=2, axes=T)
death <- rgdal::readOGR(dsn="E:\\Lectures2020\\GISC7360\\Lecture03\\Soho", 
                          layer="CholeraDeath", integer64="warn.loss")
plot(death, pch=20, col="red", add=T)

## Convert to UTM projections
UTMLondon <- CRS("+proj=utm +zone=30  +units=m")  # New coordinate sytem definition    
streets <- spTransform(streets, UTMLondon)        # Transform boundary
proj4string(streets)                              # New UTM map projection
death <- spTransform(death, UTMLondon)

## Convert streets to linnet
streetsNet <- as.linnet(streets, sparse=F)        # Also computes shortest path distances
summary(streetsNet)

## Convert death to ppp
deathPts <- as.ppp(death)
summary(deathPts)
deathPts$marks <- NULL                            # Remove marks

## Control plot
plot(streetsNet, axes=T)                          # Check projection
plot(deathPts, pch=20, add=T )

## Join network and points. Also snaps points onto the network
acciNet <- lpp(deathPts, streetsNet)
unitname(acciNet) <- c("metre", "metres")
summary(acciNet)
plot(acciNet)

## Homogeneous intensity
intensity(acciNet)                              # Average pts per meter road network
intensity(as.ppp(acciNet))                      # Average pts per square meter

## Inhomogeneous intensity with kernel density
par(mfrow=c(1,2))
system.time(acciNetDen10 <- density(acciNet, 10, continuous=T))
plot(acciNetDen10, style="colour", ribside="left")
plot(acciNetDen10, style="width", adjust=2)

system.time(acciNetDen20 <- density(acciNet, 20, continuous=T))
plot(acciNetDen20)
plot(acciNetDen20, style="width", adjust=2)

system.time(acciNetDen30 <- density(acciNet, 30, continuous=T))
plot(acciNetDen30)
plot(acciNetDen30, style="width", adjust=2)

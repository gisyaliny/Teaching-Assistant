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
sohoDeath <- lpp(deathPts, streetsNet)
unitname(sohoDeath) <- c("metre", "metres")
summary(sohoDeath)
plot(sohoDeath)

## Homogeneous intensity
intensity(sohoDeath)                              # Average pts per meter road network
intensity(as.ppp(sohoDeath))                      # Average pts per square meter

## Inhomogeneous intensity with kernel density
system.time(sohoDeathDen10 <- density(sohoDeath, 10, continuous=T))
plot(sohoDeathDen10)
plot(sohoDeathDen10, style="width", adjust=2)

system.time(sohoDeathDen20 <- density(sohoDeath, 20, continuous=T))
plot(sohoDeathDen20)
plot(sohoDeathDen20, style="width", adjust=2)

system.time(sohoDeathDen30 <- density(sohoDeath, 30, continuous=T))
plot(sohoDeathDen30)
plot(sohoDeathDen30, style="width", adjust=2)

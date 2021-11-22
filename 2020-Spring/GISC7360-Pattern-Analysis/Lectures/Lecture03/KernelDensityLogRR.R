rm(list=ls(all=TRUE))    # Clean objects from workspace

library(spatstat)        # Key library for spatial point pattern analysis 
library(smacpod)         # Relative risk kernel densities based on statstat
library(maptools)        # To open shapefiles
library(colorspace)      # Used for thematic mapping

## Importing shape-files
## Study Area Boundary
bnd.shp <- rgdal::readOGR(dsn="G:\\UTD_Classes\\2020Spring\\GISC7360_Pattern_Analysis\\Lecture03\\MaulviBazarSadar", 
                        layer="MaulviBazarSadarPoly", integer64="warn.loss")
## Cases and Control Points
pts.shp <- rgdal::readOGR(dsn="G:\\UTD_Classes\\2020Spring\\GISC7360_Pattern_Analysis\\Lecture03\\MaulviBazarSadar", 
                          layer="MaulviBazarSadar", integer64="warn.loss")
proj4string(pts.shp) 
## Converting shape-files to ppp objects
pts <- as.ppp(pts.shp)
pts <- pts[as.owin(bnd.shp)]
summary(pts)
plot(pts, main="Schools and Clinics in MaulviBazarSadar")
plot(split(pts))

## subset cases and controls
case <- subset(pts, marks=="clinics")
control <- subset(pts, marks=="school")
plot(density(case), main="24 Clinics")
plot(case, add=T)
plot(density(control), main="142 Schools")
plot(control, add=T)

## Different edge corrections
plot(density(control, edge=T, diggle=T), main="Controls with Edge Correction")

## Different bandwidth for controls (i.e., sigma)
plot(bw.ppl(control, srange=c(20,5000), ns=30), xlim=c(0,5000))      # maximum likelihood evaluation
plot(bw.ppl(control, srange=c(1500,2000), ns=30), xlim=c(1500,2000)) # maximum likelihood evaluation
plot(density(control, sigma=1710))

plot(bw.diggle(control, hmax=2000), xlim=c(0,2000))                  # diggle evaluation
plot(bw.diggle(control, hmax=1500), xlim=c(1100,1200))               # diggle evaluation
plot(density(control, sigma=bw.diggle))

bw.scott(control)                                                    # scott's (x,y)-direction evaluation
plot(density(control, sigma=bw.scott))
plot(spdensity(control, sigma=1800))
plot(control, col="green", add=T)

## Bandwidth for cases
plot(bw.diggle(case, hmax=2000), xlim=c(0,2000))
plot(density(case, sigma=bw.diggle))
plot(density(case, sigma=1500))
plot(case, add=T)

## Relative Risk evaluation with smacpod::logrr
riskMap <- logrr(pts, case=1, sigma=600, sigmacon=700) # experiment with sigma
bound <- max(abs(range(riskMap$v, na.rm=TRUE)))
plot(riskMap, main="Relative Risk for 30 Cases and 113 Controls",
     breaks=seq(-bound, bound, length.out=24+1), col=diverge_hsv(24))
plot(pts, add=T)
plot(pts$window, lwd=2, add=T)
axis(1); axis(2)

## Identify areas exessive and lower risk
riskMap <- logrr(pts, case=1, sigma=600, sigmacon=700, 
                 alternative="greater", level=0.90, nsim=99)
plot(riskMap, main="White areas are insignificant at alpha=0.10",
     breaks=seq(-bound, bound, length.out=24+1), col=diverge_hsv(24))
plot(pts, add=T)
plot(pts$window, lwd=2, add=T)
axis(1); axis(2)

## Global clustering test 
logrr.test(riskMap)

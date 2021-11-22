library(spatstat)        # Key library for spatial point pattern analysis        
library(smacpod)         # Relative risk kernel densities based on statstat
library(colorspace)
rm(list=ls(all=TRUE))    # Clean objects from workspace

## kernel smooth data will be associated
data(lansing)
pairs(density(split(lansing)[c(2, 3, 5)])) 

##
## grave site example
##
data("grave", package="smacpod")
?smacpod::grave

summary(grave)
plot(grave)
plot(split(grave))

## subset cases and controls
case <- subset(grave, marks=="affected")
control <- subset(grave, marks=="unaffected")
plot(density(case), main="30 affected")
plot(case, add=T)
plot(density(control), main="113 unaffected")
plot(control, add=T)

## Contour and perspective plot
contour(density(control), main="113 unaffected")
plot.owin(control$window, add=T)

persp(density(control), theta=0, phi= 0, main="113 unaffected")

## Different edge corrections
plot(density(control, edge=T, diggle=T), main="Controls with/without Edge Correction")

## Different bandwidth for controls (i.e., sigma)
plot(bw.ppl(control, srange=c(20,2000), ns=30), xlim=c(0,2000)) # maximum likelihood evaluation
plot(density(control, sigma=1450))

plot(bw.diggle(control, hmax=600), xlim=c(0,600))               # diggle evaluation
plot(density(control, sigma=bw.diggle))

bw.scott(control)                                               # scott's (x,y)-direction evaluation
plot(density(control, sigma=bw.scott))
plot(density(control, sigma=800))

## adaptive bandwith kernel densities
data(bei)
plot(density(bei), main="Fixed Bandwith")
plot(bei, col="green",pch=20, cex=0.4, add=T)

plot(adaptive.density(bei, f=0.01, nrep=50), main="Adaptive Bandwidth")
plot(bei, col="green", pch=20, cex=0.4, add=T)


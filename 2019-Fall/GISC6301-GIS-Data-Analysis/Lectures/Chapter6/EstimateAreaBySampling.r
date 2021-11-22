#########################################################################
##  Objective: Estimate the areas north and south of a fictive river   ##
##                                                                     ##
##  Underlying Idea: Randomly place sample points into the study area  ##
##                   Evaluate how many points are on either side of    ##
##                   the river.                                        ##
##                   The point density should be uniform within the    ##
##                   study area                                        ##
##                   The proportion of sample points on either side    ##
##                   of the river estimates the integral areas         ##
#########################################################################
rm(list=ls())
set.seed(NULL)     # reset the seed to system clock
#set.seed(pi)      # fixed sequence of random numbers

myRiver <- function(x){
## Fictitious River meandering through the study area
  y <- (max(x)-x)/4*sin((pi*x/2)^2)+0.4*x+0.4
  return(y)
} #end:myRiver

nSample <- 1000                     # Sample Size
xRange <- c(0,2)                    # bounding box
yRange <- c(0,2)
totArea <- (max(xRange)-min(xRange))*(max(yRange)-min(yRange))

## Plot River in Study Area
xSeq <- seq(xRange[1],xRange[2],by=0.001)
plot(xSeq, myRiver(xSeq), type="l" ,col="steelblue", lwd=5,
     xlab="Longitude", ylab="Latitude", 
     xlim=xRange, ylim=yRange, 
     main=bquote("Total Study Area: "*.(totArea)))

## Generate a completely random spatial pattern
## of sample points in study area
xCoord <- runif(nSample,xRange[1],xRange[2])       # Coordinates are uniform distributed
yCoord <- runif(nSample,yRange[1],yRange[2])       # thus density will be approx. uniform

## Display Results
ptsSouth <- (yCoord < myRiver(xCoord))             # Logical operation: TRUE if south
points(xCoord,yCoord,pch=3,cex=0.6,                # Plot color-coded sample points
       col=ifelse(ptsSouth == T,"red1","green1"))    # ifelse set color for north & south

xCentroidSouth <- mean(xCoord[ptsSouth])           # Get centroids south
yCentroidSouth <- mean(yCoord[ptsSouth])

xCentroidNorth <- mean(xCoord[!ptsSouth])          # Get centroids north
yCentroidNorth <- mean(yCoord[!ptsSouth])

propSouth <- sum(ptsSouth)/nSample                 # Proportion of sample points
propNorth <- sum(!ptsSouth)/nSample                # in either area

text(xCentroidSouth,yCentroidSouth,col="red4",      # Label areas at centroid
     bquote(A[S]%~~% .(totArea*propSouth)),cex=2)
text(xCentroidNorth,yCentroidNorth,col="green4",
     bquote(A[N]%~~% .(totArea*propNorth)),cex=2)


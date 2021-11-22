##########################################################
## Sample code to calculate a Weighted Euclidian Median ##
## see Burt, Barber& Rigby: Appendix 3B                 ##
## by Tiefelsdorf, 2014                                 ##
##########################################################

rm(list=ls())                       # Clean objects from workspace
#set.seed(pi*2)                      # Fixed random numbers
set.seed(NULL)                      # Pure random numbers

euclidMedian <- function(xy){
  ## Function that evaluates the cost at any point XY given
  ## the global vector argments: x, y, and w
  cost <- sum(w * sqrt( (x-xy[1])^2 + (y-xy[2])^2 ))
  return(cost)              
} #end::euclidMedian

## Set parameters
tol <- 0.0001
maxIter <- 80
nofPoints <- 10
##
## Generate random points in a 10x10 square
##
x <- runif(nofPoints,min=0,max=10)
y <- runif(nofPoints,min=0,max=10)

## Assign a random Poisson weight
w <- rpois(nofPoints,lambda=1)+1    # To avoid zeros a 1 is added

## Deterministic solution if one point hold > 50% of weights
#w[1] <- 20; x[1] <- 9; y[1] <- 9 

##
## Setup base plot
##
plot(x,y,xlim=c(-1,11), ylim=c(-1,11),             # set up coordinate system
    xlab = "x-coordinate", ylab = "y-coordinate",
    col = "blue", asp=1, cex = sqrt(w/pi)*4)       # circle size proportional to weight
## Population weighted arithmetic centroid (mean)
xWMean <- sum(w*x)/sum(w)
yWMean <- sum(w*y)/sum(w)

##
## Plot Cost Contours based on grid cells
##
xSeq <- seq(0, 10, by=0.1)         
ySeq <- seq(0, 10, by=0.1)
z <- matrix(NA,nrow=length(xSeq), ncol=length(ySeq))

for (i in 1:length(xSeq))
  for (j in 1:length(ySeq)) 
    z[i,j] <- euclidMedian(cbind(xSeq[i],ySeq[j]))         # evaluate cost at any grid cell

contour(xSeq, ySeq, z, add=T)

                                                           
points(xWMean,yWMean, col = "blue", cex=1.5, pch=19)            # add starting point to the plot
text(xWMean,yWMean," Weighted Centroid",col="blue",pos=2)

cat("\nPopulation Weighted Arithmetic Centroid:\n")
cat("xWMean = ",round(xWMean,3)," yWMean = ",round(yWMean,3),
    "Costs = ", sum(w*sqrt((x-xWMean)^2+(y-yWMean)^2)),"\n")
    
## Iteratively finding the median point
iIter <- 0
xOld <- xWMean
yOld <- yWMean
cat("\nIterations History:\n")

repeat{
    d <- sqrt((x-xOld)^2+(y-yOld)^2)                       # distance equation in step 1
    xNew <- sum(w*x/d)/sum(w/d)                            # update equation in step 2
    yNew <- sum(w*y/d)/sum(w/d)                             
    xyDiff <- sqrt((xNew-xOld)^2+(yNew-yOld)^2)            # calculate tolerance score
    xOld <- xNew
    yOld <- yNew
    iIter <- iIter + 1
    if (xyDiff < tol | iIter > maxIter) break
    Sys.sleep(0.5)                                         # Pause 0.5 sec
    points(xNew,yNew,col="red", cex=1)                     # plot updated point
    cat("i: ",iIter," x: ",round(xNew,3)," y: ",round(yNew,3),
        " Cost: ",sum(w*sqrt((x-xNew)^2+(y-yNew)^2)),"\n")
    } #end::repeat

points(xNew, yNew, col="red", cex=3, pch=3)                # plot final weighted median
text(xNew,yNew," Weighed Median",col="red",pos=4)

cat("\nFinal Iteration Results:\n")
cat("xMedian = ",round(xNew,3)," yMedian = ",round(yNew,3),
    "Min. Cost= ",sum(w*d),"\n")


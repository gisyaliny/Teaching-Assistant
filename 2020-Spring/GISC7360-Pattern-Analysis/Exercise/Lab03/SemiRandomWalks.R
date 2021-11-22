library(circular)
rm(list=ls(all=TRUE))                         # Clean objects from workspace

path <- rep(c(rep("G",50),rep("R",50)),2)

## Define functions to model specific processes
dirMC <- function(iStep, dirOld, kappaOld) {
  if (path[iStep] == "G") dirNew <- rvonmises(1, mu=dirOld, kappa= kappaOld)
  #if (path[iStep] == "R") dirNew <- rvonmises(1, mu=dirOld, kappa= kappaOld)
  if (path[iStep] == "R") dirNew <- circular(pi/4)
  #dirNew <- rvonmises(1, mu=dirOld, kappa=kappaOld)
  return(dirNew)
  } #end::dirMC

lenMC <- function(iStep, lenOld) {
  if (path[iStep] == "G") lenNew <- runif(1, min=0, max=4)
  if (path[iStep] == "R") lenNew <- runif(1, min=2, max=4)
  #lenNew <- runif(1, min=0, max=1)
  return(lenNew)
  } #end::lenMC

kappaMC <- function(iStep, kappaOld) {
  if (path[iStep] == "G") kappaNew <- runif(1, min=0, max=0.5)
  if (path[iStep] == "R") kappaNew <- runif(1, min=4, max=6)
  kappaNew <- kappaOld
  return(kappaNew)
} #end::kappaMC

## Initialize parameters
nSteps <- 200                # number of steps
kappaOld <- 0                # von Mises distribution intial concentration
dirOld <- circular(pi/4)     # Initial direction        
lenOld <- 1.0                # Initial step length
i <- 1
## Matrix storing [a] directions, [b] step length for each step and [b] kappa
dl <- matrix(0, nrow=nSteps, ncol=3)         
dl[1, 1] <- dirOld
dl[1, 2] <- lenOld
dl[1, 3] <- kappaOld

for (i in 2:nSteps) {
  ## Update parameters for first order Markov chains
  ## Make dir, len and kappa functions of the the step number and/or previous step parameters
  dirOld <- dl[i, 1] <- dirMC(i, dirOld, kappaOld)  
  lenOld <- dl[i, 2] <- lenMC(i, lenOld)
  kappaOld <- dl[i, 3] <- kappaMC(i, kappaOld)
} # end:Step

## Calculate coordinates and path 
xy <- cbind(cos(dl[,1])*dl[,2], sin(dl[,1])*dl[,2])
xy <- cbind(cumsum(xy[,1]), cumsum(xy[,2]))

layout(matrix(c(1,2,3),nrow=3), heights=c(2,1,1))    # Setup canvas for plots

## Plot path
plot(xy, pch=20, cex=1.0,col="yellowgreen", asp=1,        
     xlab="x", ylab="y", main="Semi-Random Walk")     
lines(xy)                                                
points(xy[1,1], xy[1,2], pch=16, cex=1.5, col="green")             # start point
points(xy[nSteps,1], xy[nSteps,2], pch=15, cex=1.3, col="red")     # start point


## Calculate turning angles. See Fortin et al. p 277
#a1 <- pi/2; a2 <- pi; a3 <- 0;a4 <- 3*pi/2
#cos(a1-a1); cos(a2-a1); cos(a3-a1); cos(a4-a1)
dl[2:nSteps,1] <- cos(dl[2:nSteps,1]-dl[1:nSteps-1,1])

## Calculate autocorrelation functions
acf(dl[3:nSteps ,1], demean = F, lag.max=30, main="AC for Turning Angles")
acf(dl[3:nSteps ,2], lag.max=30, main="AC for Step Length")

layout(1)                                           # reset canvas
rm(list=ls())                          # Clear environment
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

################################################################################
## Demonstrates numerical integration to get 
## [a] the distribution function from the density function
## [b] calculate the expectation
## [c] calculate the variance
##
## Example: the exponential distribution with x >= 0 and parameter lambda
##          density <- lambda*exp(-lambda*x)
##          distribution <- 1 - exp(-lambda*x)
##          expectation  <- 1/lambda
##          variance <- 1/lambda^2
##
##          Estimator lambda <- 1/mean(x)
################################################################################

IntBoxes <- function(IntFunc,a,b,n,plotIt=TRUE){
  ## plots the Rieman summands into an existing plot
  ## Calculate the midpoint Riemann sum
  ## start value <- a
  ## end value <- b
  ## number of summands <- n in interval a to b
  intgrnd <- match.fun(IntFunc)
  integrand <- function(x) intgrnd(x)
  xleft <- seq(a,b-((b-a)/n),by=(b-a)/n)
  xright <- seq(a+((b-a)/n),b,by=(b-a)/n)
  ybottom <- rep(0,n-1)
  ytop <- integrand(seq(a+((b-a)/(2*n)),b-((b-a)/(2*n)),by=(b-a)/n))
  if (plotIt) rect(xleft,ybottom,xright,ytop,col="grey")      # plot summands
  RieSum <- (b-a)/n*sum(ytop)
  return(RieSum)
} #end::IntBox

  ## Parameters
  nBoxes <- 20      # Number of Summands for the Rieman sum
  lambda <- 1       # Parameter of exponential distribution: lambda with E[x]=1/lambda and Var[x]=1/lambda^2
  xMin <- 0         # Lower integration bound
  xMax <- 10        # Upper integration bound (truncation point because x in 0 to infinity). Set xMax larger for smaller lambdas 
  x <- seq(xMin,xMax,length.out=500) # Sequence of x values just to get a smooth plot
  
  ## Define function to be evaluated
  ExpDens <- function(x) {                     # density
    ifelse(x >= 0, lambda*exp(-lambda*x), 0)
  }
  ExpDensExpect <- function(x) {               # expected value
    ifelse(x >= 0, x * ExpDens(x), 0)
  }
  
  ExpDensVar <- function(x) {                  # variance
    ifelse(x >= 0, (x-1/lambda)^2 * ExpDens(x), 0)
  }
##
## Evaluate Integrals of the exponential distribution
##
## Distribution function within a specific range
xCut <- xMax
plot(x,ExpDens(x),type="n",xlab="x-value",
     ylab=bquote(paste(f(x),"  at  ", lambda %==% .(lambda))))
abline(v=0,lty="dotted",col="grey"); abline(h=0,lty="dotted",col="grey")
ExpDistrib <- IntBoxes(ExpDens,xMin, xCut, nBoxes)
lines(x,ExpDens(x),type="l",col="red",lwd=3)
title(main=bquote(paste("Distribution: ",integral(lambda%.%exp(-lambda%.%x)%.%dx, .(xMin), .(xCut)) %~~% .(round(ExpDistrib,5)))))

## Expected value
plot(x,ExpDensExpect(x),type="n", xlab="x-value",
     ylab=bquote(paste(x%.%f(x),"  at  ", lambda %==% .(lambda))))
abline(v=0,lty="dotted",col="grey"); abline(h=0,lty="dotted",col="grey")
EstExpect <- IntBoxes(ExpDensExpect,xMin,xCut,nBoxes)
lines(x,ExpDensExpect(x),type="l",col="red",lwd=3)
title(main=bquote(paste("Expectation: ",integral(x%.%f(x)%.%dx, .(xMin), .(xCut)) %~~% .(round(EstExpect,5)))))

## Variance
plot(x,ExpDensVar(x),type="n",xlab="x-value",
     ylab=bquote(paste((x-1/lambda)^2%.%f(x),"  at  ", lambda %==% .(lambda))))
abline(v=0,lty="dotted",col="grey"); abline(h=0,lty="dotted",col="grey")
EstVar <- IntBoxes(ExpDensVar,xMin,xCut,nBoxes)
lines(x,ExpDensVar(x),type="l",col="red",lwd=3)
title(main=bquote(paste("Variance: ",integral((x-over(1,lambda))^2%.%f(x)%.%dx, .(xMin), .(xCut)) %~~% .(round(EstVar,5)))))

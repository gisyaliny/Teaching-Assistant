rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

library(AER); library(fields)
data("CPS1985")
##
## Demonstration that one can get a perfect R^2 by random regressors
##
summary(lm(wage~education+experience, data=CPS1985))
( n <- nrow(CPS1985) )
set.seed(6)                               # set starting value of RNG
stdNorm <- matrix(rnorm(n*(n-2)), nrow=n)  # matrix of random numbers
funnyReg <- lm(wage~stdNorm, data=CPS1985)
summary(funnyReg)
#car::vif(funnyReg)

##
## Investigate degree of linear dependence
##
corMat <- cor(stdNorm)
det(corMat)
eigval <- eigen(corMat, only.values = T)$values
plot(eigval, type="l", main="Eigenvalue Spectrum")
abline(h=c(0,1), col=c("black","red"), lty=c(1,5))

##
## Thin-Plate Spline Smoothing
##
wage <- as.vector(CPS1985[, 1])
X <- as.matrix(CPS1985[, 2:3])
polysmooth <- Tps(X, wage, df=100)     # change df from 3 thru 100
surface(polysmooth, main="Thin-Plate Smoother")

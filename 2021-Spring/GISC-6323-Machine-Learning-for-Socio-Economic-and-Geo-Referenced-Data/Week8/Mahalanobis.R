#################################################
## Script demonstrating the underlying principle 
## of the Mahalanobis distance
#################################################
rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

## 
## Generate correlated Observations with X1 and X2 being redundant
##
n <- 10                               # number of observations
mu <- rep(0,3)                        # mean of three variables
## Co-variance matrix
cMat <- matrix(c(1.0,1.0,0.0,1.0,1.0,0.0,0.0,0.0,1.0), nrow=3,ncol=3)
#cMat <- matrix(c(1.0,0.9,0.0,0.9,1.0,0.0,0.0,0.0,1.0), nrow=3,ncol=3)
## Simulate multivariate normal distribution with these parameters
cMat
x <- MASS::mvrnorm(n, mu, cMat, empirical=TRUE)
colnames(x) <- c("X1","X2","X3")
rownames(x) <- paste("O",1:n, sep="")
View(x)
round(var(x),4)                       # empirical co-variance matrix

## Visualize the three variables
car::scatter3d(x[,1], x[,2], x[,3],
               xlab="X1", ylab="X2", zlab="X3", surface=FALSE, residuals=FALSE)

## Caculate distance matrix with correlated variables
round(dCor <- dist(x, diag=TRUE, upper=TRUE), 2)

## Caculate distance matrix with uncorrelated variables, i.e., X2 dropped
round(dMaNo <- dist(x[, -2], diag=TRUE, upper=TRUE), 2)

##
## Calculate equivalent Mahalanobis distance using principal components
##

## Principal Component Analysis
prX <- prcomp(x, retx=TRUE, center=TRUE, scale.=TRUE)
round(prX$sdev^2, 4)       # Eigenvalues
round(var(prX$x), 4)       # Evaluate co-variance of principal components

## Compared distance with correlated variables
round(PCdCor <- dist(prX$x[,1:2], diag=TRUE, upper=TRUE), 2)
all.equal(PCdCor, dCor, check.attributes=FALSE)

## Compared distance with uncorrelated variables
pcScaled <- scale(prX$x[,1:2])
round(pcMaNo <- dist(pcScaled, diag=TRUE, upper=TRUE), 2)
all.equal(pcMaNo, dMaNo, check.attributes=FALSE)

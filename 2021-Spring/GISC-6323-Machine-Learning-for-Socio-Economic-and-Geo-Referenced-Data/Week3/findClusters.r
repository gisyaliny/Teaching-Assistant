rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

##
## Find normally distributed clusters in a set of random points
##
#install.packages("mclust")
library(mclust)

## Generate 3 random clusters
spread <- 0.5
nofpts1 <- 20
x1 <- rnorm(nofpts1, mean=2, sd=spread)
y1 <- rnorm(nofpts1, mean=2, sd=spread)

nofpts2 <- 35
x2 <- rnorm(nofpts2, mean=6, sd=spread)
y2 <- rnorm(nofpts2, mean=2, sd=spread)

nofpts3 <- 45
x3 <- rnorm(nofpts3,mean=4, sd=spread)
y3 <- rnorm(nofpts3,mean=6, sd=spread)

X <- c(x1,x2,x3)
Y <- c(y1,y2,y3)

p <- data.frame(x=c(x1,x2,x3), y=c(y1,y2,y3))
plot(Y~X, data=p, pch=20, cex=2, asp=1,
     col=c(rep("blue",nofpts1),rep("red",nofpts2),rep("green",nofpts3)),
     main="Distribution of Points in 3 Clusters")

## Estimate clusters with 2,3, and 4 suggested groups
p.Mclust <- Mclust(p, G=c(2,3,4), modelNames="VVV")
summary(p.Mclust)

## Plot of "optimal" model with 3 clusters
plot(p.Mclust, what="BIC")
plot(p.Mclust, what="classification")
plot(p.Mclust, what="uncertainty")
plot(p.Mclust, what="density")

p.Mclust$parameters

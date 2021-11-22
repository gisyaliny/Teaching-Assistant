rm(list=ls(all=TRUE))               # Clean objects from workspace
##
## Find normally distributed clusters in a set of random points
##
library(mclust)

## Generate 3 random clusters
nofpts1 <- 20
x1 <- runif(nofpts1,min=0,max=3)
y1 <- runif(nofpts1,min=0,max=6)

nofpts2 <- 35
x2 <- runif(nofpts2,min=4,max=6)
y2 <- runif(nofpts2,min=4,max=6)

nofpts3 <- 45
x3 <- runif(nofpts3,min=5,max=10)
y3 <- runif(nofpts3,min=5,max=10)

X <- c(x1,x2,x3)
Y <- c(y1,y2,y3)

p <- data.frame(X,Y)
plot(Y~X, data=p, pch=20, cex=2,
     col=c(rep("blue",nofpts1),rep("red",nofpts2),rep("green",nofpts3)),
     main="Distribution of Points in three Clusters")

## Estimate clusters with 2,3, and 4 groups
p.Mclust <- Mclust(p, G=c(2,3,4), modelNames="VVV")
summary(p.Mclust)

## Plot of "optimal" model with 3 clusters
plot(p.Mclust, what="BIC")
plot(p.Mclust, what="classification")
plot(p.Mclust, what="uncertainty")
plot(p.Mclust, what="density")

p.Mclust$parameters

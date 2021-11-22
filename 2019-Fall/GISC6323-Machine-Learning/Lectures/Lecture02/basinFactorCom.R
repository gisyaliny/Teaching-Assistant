rm(list=ls())
library(foreign); library(car)
setwd("E:\\Lectures2019\\GISC6323\\Lecture02")
basin <- read.dta("basins.dta") 
names(basin)

scatterplotMatrix(~yield+runoff+precip+glacier+area, data=basin)

basin[,6] <- basin[,6] + 1              # glacier has-zeros. log wont work
basin[,3:7] <- log(basin[,3:7])         # perform log-transformation
scatterplotMatrix(~yield+runoff+precip+glacier+area, data=basin)

corBasin <- cor(basin[,3:6])            # correlation matrix
print(corBasin, digits=2)

##
## Principal component analysis on correlation matrix including component scores
##
?princomp # study online help
pcBasin <- princomp(~runoff+precip+glacier+area, data=basin, cor=TRUE, scores=TRUE)
summary(pcBasin, loadings=TRUE, cutoff=0.5)

apply(pcBasin$loadings^2,1,sum)         # row-sum of squared loadings

## Book results p 255
round(pcBasin$sdev^2, 2)                # eigenvalues = variance explained by component

pcLoad <- pcBasin$loadings %*% diag(pcBasin$sdev) # loadings (colums scaled by eigenvalue)
print(pcLoad, digits=2)
apply(pcLoad^2,2,sum)                             # squared loadings of factors = variance explained

## Component scores
( pcScores <- pcBasin$scores )          # observation value associated with each component
print(round(cor(pcScores), 2))          # uncorrelated component scores
apply(pcScores,2,var) * 18/19           # Each component has the eigenvalue as variance

( pcLoad2 <- cor(basin[,3:6],pcScores[,1:2]) ) # see loading Table 8.3 with first two components

## Descriptive plots
screeplot(pcBasin, type="lines")        # Scree graph Fig 8.4
abline(h=1,lty=2)
biplot(pcBasin)                         # unrotated components
abline(h=0,v=0, lty=5)

##
## Principle component analysis using matrix expressions
##
zBasin <- scale(basin[,3:6])    
zBasin
scatterplotMatrix(zBasin)   # see Fig 8.3

( corBasin <- t(zBasin) %*% zBasin /18)
eigSys <- eigen(corBasin, symmetric=TRUE)   # eigen-system decomposition of correlation matrix
eigVal <- eigSys$values                     # eigenvalues
eigVec <- eigSys$vectors                    # eigenvectors equivalent with rotation matrix A
pcScores <- zBasin %*% eigVec               # Component scores 
( pcLoad <- cor(zBasin,pcScores[,1:2]) )

cat("Eigenvalues:\n");eigVal
cat("Percent Explained Variance:\n");cumsum(eigVal)/sum(eigVal)
cat("Rotation A:\n");eigVec
cat("Component Scores:\n");round(pcScores,3)
cat("Componet Loadings:\n");pcLoad

##
## Regression with Components
##
org.lm <- lm(yield~runoff+precip+glacier+area, data=basin)
summary(org.lm)
vif(org.lm)

basinWithComp <- data.frame(basin, pcScores)
comp.lm <- lm(yield~X1+X2, data=basinWithComp)
summary(comp.lm)


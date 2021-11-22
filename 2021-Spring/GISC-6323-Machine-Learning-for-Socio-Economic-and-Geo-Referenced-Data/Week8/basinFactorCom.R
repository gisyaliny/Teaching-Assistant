rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

library(car); library(pls)
# setwd("E:\\Lectures2021\\GISC6323\\Week08")
basin <- foreign::read.dta("basins.dta") 
View(basin)

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

## Eigenvalues of each component, i.e., explained variances
round(pcBasin$sdev^2, 2)                

## Component scores
( pcScores <- pcBasin$scores )          # observation value associated with each component
print(round(cor(pcScores), 2))          # uncorrelated component scores
apply(pcScores,2,var) * 18/19           # Each component has the eigenvalue as variance

## Descriptive plots
screeplot(pcBasin, type="lines")        # Scree graph Fig 8.4
abline(h=1,lty=2)
biplot(pcBasin)                         # unrotated components
abline(h=0,v=0, lty=5)


##
## Regression with First and Second Principal Components
##
org.lm <- lm(yield~runoff+precip+glacier+area, data=basin)
summary(org.lm)
vif(org.lm)

basinWithComp <- data.frame(basin, pcScores)
comp.lm <- lm(yield~Comp.1+Comp.2, data=basinWithComp)
summary(comp.lm)

##
## Principal Component Regression
##
pcr.lm <- pcr(yield~runoff+precip+glacier+area, data=basin, scale=TRUE, 
              validation="CV", ncomp=4)
summary(pcr.lm)
validationplot(pcr.lm)

##
## Partial Least Squares Regression
##
plsr.lm <- plsr(yield~runoff+precip+glacier+area, data=basin, scale=TRUE, 
              validation="CV", ncomp=4)
summary(plsr.lm)
validationplot(plsr.lm)
yieldPredict <- predict(plsr.lm, ncomp=1)
cor(yieldPredict, basin$yield)^2     # model R^2

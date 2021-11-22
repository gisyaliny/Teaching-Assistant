#install.packages("class")                      # install package with kNN
#install.packages("gmodels")                    # for nice cross-tabulations
rm(list=ls())
library(class); library(gmodels)

wbcd <- read.csv("E:\\Lectures2019\\GISC6323\\Lecture01\\wisc_bc_data.csv", 
                 stringsAsFactors = FALSE)
wbcd <- wbcd[,-1]

car::scatterplotMatrix(~radius_mean+area_mean+smoothness_mean, data=wbcd,
                       groups=wbcd$diagnosis)

round(cor(wbcd[,-1]),2)  # high correlation indicates redundancy

car::scatter3d(wbcd$radius_mean, wbcd$area_mean, wbcd$smoothness_mean,
               xlab="Radius", ylab="area", zlab="smoothness",
               surface=FALSE, residuals=FALSE,
               point.col=ifelse(wbcd$diagnosis=="Benign", "green", "red"))

##
## Performing a principal Component Analysis on features
##
wbcd_pc <-prcomp(wbcd[-1], retx=TRUE, scale.=TRUE)
str(wbcd_pc)

## Scree plot
plot(wbcd_pc$sdev^2, type="b", main="Scree Plot", 
     xlab="Component Order", ylab="Eigenvalue")
abline(h=1, v=1, lty=5)
nofComp <- 6                      # Number of selected components

## Explained Variance
cumVar <- cumsum(wbcd_pc$sdev^2)/length(wbcd_pc$sdev)*100
plot(cumVar, main="Percent Explained Variance",
     type="b", xlab="Component Order", ylab="Relative Cummulative Variance")
abline(v=nofComp, h=cumVar[nofComp], lty=5)

## Visualize first three components
car::scatter3d(wbcd_pc$x[, 1], wbcd_pc$x[, 2], wbcd_pc$x[, 3],
               xlab="1st Comp.", ylab="2nd Comp.", zlab="3rd Comp.",
               surface=FALSE, residuals=FALSE,
               point.col=ifelse(wbcd$diagnosis=="Benign", "green", "red"))

## 
## Using the first 6 components for kNN
##

trainLab <- wbcd[1:469, 1]    # analogue split labels
testLab <- wbcd[470:569, 1]

trainPC <- wbcd_pc$x[1:469, 1:6]
testPC <- wbcd_pc$x[470:569, 1:6]

## re-classify test cases
testPred <- knn(train = trainPC, test = testPC,
                cl = trainLab, k = 20)

## Create the cross tabulation of predicted vs. actual for z-transformation
CrossTable(x = testLab, y = testPred, prop.chisq = FALSE)


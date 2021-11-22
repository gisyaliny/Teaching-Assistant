rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

#install.packages("class")             # install package with kNN
#install.packages("gmodels")           # for nice cross-tabulations

library(class); library(gmodels)

## create normalization function to make feature dimensions comparable
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


## 
## Import the CSV file. Visit http://archive.ics.uci.edu/ml/ for more information
## The fractal dimension is the relationship between the change in measurement scale
## to the change in measured perimeter length of a complex shape. 
## See: https://en.wikipedia.org/wiki/Fractal_dimension 
##
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)

## examine wbcd data frame
View(wbcd)

## drop the id feature
wbcd <- wbcd[-1]

## recode diagnosis as a factor
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

## table or proportions with more informative labels
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

## Explore three numeric features
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])
with(wbcd, {
  boxplot(radius_mean~diagnosis, xlab="radius_mean")
  boxplot(area_mean~diagnosis, xlab="area_mean")
  boxplot(smoothness_mean~diagnosis, xlab="smoothness_mean")
} )
car::scatterplotMatrix(~radius_mean+area_mean+smoothness_mean, data=wbcd,
                       groups=wbcd$diagnosis)
car::scatter3d(wbcd$radius_mean, wbcd$area_mean, wbcd$smoothness_mean,
               xlab="Radius", ylab="area", zlab="smoothness",
               surface=FALSE, residuals=FALSE,
               point.col=ifelse(wbcd$diagnosis=="Benign", "green", "red"))

##
## Perform kNN classifications
##

## normalize the wbcd data to make feature scales comparable
## important for all distance-based measures
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n)

## split the dataframe into a training and a test sample
## check also sample function
## trainIdx <- sample(1:569, size=469, replace=FALSE)
## testIdx <- setdiff(1:569, trainIdx)

trainN <- wbcd_n[1:469, -1]          # split features
testN <- wbcd_n[470:569, -1]

trainLab <- wbcd[1:469, 1]    # analogue split labels
testLab <- wbcd[470:569, 1]

testPred <- knn(train = trainN, test = testN, prob = TRUE,
                      cl = trainLab, k = 21)
testPred

## Create the cross tabulation of predicted vs. actual for normalization
data.frame(Observed=testLab, Predicted=testPred,    # check records 32 and 54
           PredProb=attr(testPred, "prob"))
CrossTable(x = testLab, y = testPred, prop.chisq = FALSE)

##
## Alternative feature scaling: use of z-transformation
##
wbcd_z <- as.data.frame(scale(wbcd[-1]))

trainZ <- wbcd_z[1:469, -1]
testZ <- wbcd_z[470:569, -1]

## re-classify test cases
testPred <- knn(train = trainZ, test = testZ,
                      cl = trainLab, k = 21)

## Create the cross tabulation of predicted vs. actual for z-transformation
CrossTable(x = testLab, y = testPred, prop.chisq = FALSE)

##
## Experiment with different hyper-parameters k=1:35
##
misClass <- NULL
kSeq <- 1:100
for (k in kSeq){
  testPred <- knn(train = trainN, test = testN, prob = TRUE,
                   cl = trainLab, k = k)
  misClass <- c(misClass, sum(abs(unclass(testLab)-unclass(testPred))))
}
misClass <- misClass/length(trainN)    # Error rate
plot(misClass~kSeq, type="l", main="Misclassification by # of Nearest Neighbors")
abline(v=which.min(misClass), col="red", lty=5)
##
## Performing a principal Component Analysis on features
##
wbcd_pc <-prcomp(wbcd[,-1], retx=TRUE, scale.=TRUE)
str(wbcd_pc)

## Scree-plot
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
trainPC <- wbcd_pc$x[1:469, 1:6]
testPC <- wbcd_pc$x[470:569, 1:6]

## re-classify test cases
testPred <- knn(train = trainPC, test = testPC,
                cl = trainLab, k = 20)

## Create the cross tabulation of predicted vs. actual for z-transformation
CrossTable(x = testLab, y = testPred, prop.chisq = FALSE)


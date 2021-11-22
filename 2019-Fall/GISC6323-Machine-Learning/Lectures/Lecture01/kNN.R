#install.packages("class")                      # install package with kNN
#install.packages("gmodels")                    # for nice cross-tabulations
rm(list=ls())
library(class); library(gmodels)

## create normalization function to make feature dimensions comparable
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


## 
## Import the CSV file. Visit http://archive.ics.uci.edu/ml/ for more information
## The fractal dimension is the relationship between the change in measurment scale
## to the change in measured perimeter lenght of a complex shape. 
## See: https://en.wikipedia.org/wiki/Fractal_dimension 
##
wbcd <- read.csv("E:\\Lectures2019\\GISC6323\\Lecture01\\wisc_bc_data.csv", 
                 stringsAsFactors = FALSE)

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
  boxplot(radius_mean, xlab="radius_mean")
  boxplot(area_mean, xlab="area_mean")
  boxplot(smoothness_mean, xlab="smoothness_mean")
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
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n)

## split the dataframe into a training and a test sample
## check also sample function
## trainIdx <- sample(1:569, size=469, replace=FALSE)
## testIdx <- setdiff(1:569, trainIdx)

trainN <- wbcd_n[1:469, ]          # split features
testN <- wbcd_n[470:569, ]

trainLab <- wbcd[1:469, 1]    # analogue split labels
testLab <- wbcd[470:569, 1]

testPred <- knn(train = trainN, test = testN, prob = TRUE,
                      cl = trainLab, k = 21)
testPred

## Create the cross tabulation of predicted vs. actual for normalization
data.frame(Observed=testLab, Predicted=testPred,    # check records 32 and 54
           PredProb=round(attr(testPred, "prob"),3))
CrossTable(x = testLab, y = testPred, prop.chisq = FALSE)

##
## Alternative feature scaling: use of z-transformation
##
wbcd_z <- as.data.frame(scale(wbcd[-1]))

trainZ <- wbcd_z[1:469, ]
testZ <- wbcd_z[470:569, ]

## re-classify test cases
testPred <- knn(train = trainZ, test = testZ,
                      cl = trainLab, k = 21)

## Create the cross tabulation of predicted vs. actual for z-transformation
CrossTable(x = testLab, y = testPred, prop.chisq = FALSE)

##
## Experiment with different hyper-parameters k=1,5,10,15,20,25,30
##
testPred <- knn(train = trainN, test = testN, prob = TRUE,
                cl = trainLab, k = 1)
CrossTable(x = testLab, y = testPred, prop.chisq = FALSE)

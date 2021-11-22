rm(list=ls())
#set.seed(468215, sample.kind = "Rounding")
set.seed(468215)
library(DallasTracts); library(class); library(gmodels)

## create normalization function to make feature dimensions comparable
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data(tractShp)
tractShp$post1980 <- tractShp$PCTB1980+tractShp$PCTB1990+tractShp$PCTB2000+tractShp$PCTB2010

validTractShp <- tractShp[!is.na(tractShp$BUYPOW), ]         # Remove 2 airport tracts with NA's

hist(validTractShp$post1980)
mapBiPolar(validTractShp$post1980, validTractShp, break.value=50,
           neg.breaks=6, pos.breaks=4,
           map.title="Proportiong of Homes Built After 1980",
           legend.title="Percent built\npost 1980")
plot(lakesShp, col="skyblue", border="skyblue",add=T)
plot(hwyShp, col="cornsilk3", lwd=3, add=T)
plot(bndShp, border="black", add=T)
box()

validTractShp$NewOld <- as.factor(ifelse(validTractShp$post1980 > 50, "new", "old"))
summary(validTractShp)

## Drop non-metric and irrelevant variables and those with missing observations
varDrop <- c("ID","TRACT","DES3NEIG","FOODDES","CITYPERI","TIME2WORK","MEDFAMINC",
             "MEDVALHOME","PCTB2010","PCTB2000","PCTB1990","PCTB1980","PCTB1970",
             "PCTB1960","PCTB1950","PCTB1940","PCTBPRE","SeqId","nCells","post1980",
             "NewOld")
xVars <- as.data.frame(validTractShp)
xVars[varDrop] <- NULL

## Set target variable
yVar <- validTractShp$NewOld

##
## Task 1.1: Normalize variables
##
xVars <- as.data.frame(lapply(xVars, normalize))
summary(xVars)

## Define training and testing samples
testSel <- sample(1:length(yVar), size=100)
yVarTest <- yVar[testSel]
xVarsTest <- xVars[testSel, ]
yVarTrain <- yVar[-testSel]
xVarsTrain <- xVars[-testSel, ]

## Map Test and Training Samples
validTractShp$Sample <- "Train"
validTractShp$Sample[testSel] <- "Test"
validTractShp$Sample <- as.factor(validTractShp$Sample)
mapColorQual(validTractShp$Sample, validTractShp, map.title="Training & Test Samples",
             legend.title="Samples")

##
## Task 1.2: Evaluate kNN for a range of k=values
##
result <- NULL
for (i in 1:60){
  testPred <- knn(train = xVarsTrain, test = xVarsTest, prob = TRUE,
                  cl = yVarTrain, k = i)
  totalErr <- sum(yVarTest != testPred)
  falsePos <- sum(yVarTest[yVarTest=="new"] != testPred[yVarTest=="new"])
  falseNeg <- sum(yVarTest[yVarTest=="old"] != testPred[yVarTest=="old"])
  result <- rbind(result, c(i, totalErr, falsePos, falseNeg))  
}

## Identify k-value with lowest total error
(kValue <- which.min(result[, 2]))

## Plot error counts by # of NN
layout(matrix(c(1,2,3), nrow=3))
plot(result[,c(1,2)], type="b", xlab="# of NN", ylab="Total Error", 
     main="Total Number of Errors", ylim=c(0,25))
     abline(v=kValue,col="red")
plot(result[,c(1,3)], type="b", xlab="# of NN", ylab="False Positive Count", 
     main="False Positive Count", ylim=c(0,25))
     abline(v=kValue,col="red")
plot(result[,c(1,4)],type="b", xlab="# of NN", ylab="False Negative Count", 
     main="False Negative Count", ylim=c(0,25))
     abline(v=kValue,col="red")
layout(1)


##
## Task 1.3: Evaluate error rates
##
testPred <- knn(train = xVarsTrain, test = xVarsTest, prob = TRUE,
                cl = yVarTrain, k = kValue)
CrossTable(x = yVarTest, y = testPred, prop.c=FALSE, prop.T=FALSE, prop.chisq = FALSE)

##
## Task 1.4: Evaluate predicted probabilites
##
predProbs <- ifelse(testPred == "old", 1-attr(testPred, "prob"), attr(testPred, "prob")  )
plot(validTractShp$post1980[testSel], predProbs, xlab="Proportion of newer homes",
     ylab="Predicted Probablity of new homes", main="Newer Versus Older Homes by Test Sample Census Tract")
abline(lm(predProbs~validTractShp$post1980[testSel]), lty=5, lwd=3, col="red")
abline(v=50, h=0.5, lty=3, lwd=2, col="salmon")

##
## Task 2.1: PC analysis
##
pc <-prcomp(xVars, retx=TRUE, scale.=TRUE)
plot(pc$sdev^2, type="b", main="Scree Plot", 
     xlab="Component Order", ylab="Eigenvalue")
abline(h=1, v=c(3,7), lty=5,lwd=2, col="red")

##
## Task 2.2: Use kNN with 1 to 8 component score vectors. Since the variance of 
##           the vectors is equal to the associated eigenvalue the vectors need
##           to be normalized.
##
newVars <- pc$x[, 1:8]
newVars <- as.data.frame(apply(newVars, 2, normalize))

xVarsTest <- newVars[testSel, ]
xVarsTrain <- newVars[-testSel, ]
result <- NULL
for (i in 1:8){
  testPred <- knn(train = xVarsTrain, test = xVarsTest, prob = TRUE,
                  cl = yVarTrain, k = 30)
  totalErr <- sum(yVarTest != testPred)
  result <- rbind(result, c(i, totalErr))  
}

## Identify k-value with lowest total error
(kValue <- which.min(result[, 2]))
plot(result[,c(1,2)], type="b", xlab="# of NN", ylab="Total Error", 
     main="Total Number of Errors", ylim=c(0,25))
abline(v=kValue,col="red")

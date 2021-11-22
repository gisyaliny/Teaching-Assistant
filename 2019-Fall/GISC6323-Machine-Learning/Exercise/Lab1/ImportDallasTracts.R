rm(list=ls())
library(DallasTracts)
set.seed(468215)
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
           map.title="Proportion of Homes Built After 1980",
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

### Task.1.1
##################
for (coln in colnames(xVars)) {
  xVars[coln] <- normalize(xVars[coln])
}

## Set target variable
yVar <- validTractShp$NewOld

## Define training and testing samples
testSel <- sample(1:length(yVar), size=100)
yVarTest <- yVar[testSel]
xVarsTest <- xVars[testSel, ]
yVarTrain <- yVar[-testSel]
xVarsTrain <- xVars[-testSel, ]

### Task.1.2
##################
library(class); library(gmodels)

error_lst <- c()
for (i in seq(5,60,by = 5)) {
  k_accuracy <- caret::confusionMatrix( yVarTest,knn(train = xVarsTrain, test = xVarsTest,cl = yVarTrain, k = i))$overall["Accuracy"]
  error_rate <- 1 - k_accuracy
  error_lst <- c(error_lst,error_rate)
  }
plot( seq(5,60,by = 5),error_lst,"l")

testpred <- knn(train = xVarsTrain, test = xVarsTest,cl = yVarTrain, k = 30,prob = T)
result <- cbind(as.character(yVarTest),as.character(testpred),round(attr(testpred, "prob"),2))
colnames(result) <- c("Observed","Predicted","Accuracy")
result <- data.frame(result)
result_new <- subset(result,result$Observed == "new")
index <- result_new$Observed == result_new$Predicted
result_new$Accuracy <- dmm::unfactor(result_new$Accuracy)
result_new$Accuracy[!index] <- 1 - result_new$Accuracy[!index]
result_new


validTractShp$post1980[testSel]
yVarTest
caret::confusionMatrix(yVarTest,testpred)
ta <- CrossTable(x = yVarTest,y = testpred)
stargazer(ta)
??stargazer
attr(a, "prob")

xVars_pc <-prcomp(xVars, retx=TRUE, scale.=TRUE)
plot(xVars_pc$sdev^2, type="b", main="Scree Plot", 
     xlab="Component Order", ylab="Eigenvalue")


set.seed(12345)
set <- 1:100
sample(set,10)

testPred <- knn(train = xVarsTrain, test = xVarsTest,cl = yVarTrain, k = 20)
attr(testPred,"prob")
?attr
## Map Test and Training Samples
validTractShp$Sample <- "Train"
validTractShp$Sample[testSel] <- "Test"
validTractShp$Sample <- as.factor(validTractShp$Sample)
mapColorQual(validTractShp$Sample, validTractShp, map.title="Training & Test Samples",
             legend.title="Samples")


### Task 2.1

xVars_pc <-prcomp(xVars, retx=TRUE, scale.=TRUE)
plot(xVars_pc$sdev^2, type="b", main="Scree Plot", 
     xlab="Component Order", ylab="Eigenvalue")
abline(h=1, v=1, lty=5)
nofComp <- 8

## Explained Variance
cumVar <- cumsum(xVars_pc$sdev^2)/length(xVars_pc$sdev)*100
plot(cumVar, main="Percent Explained Variance",
     type="b", xlab="Component Order", ylab="Relative Cummulative Variance")
abline(v=nofComp, h=cumVar[nofComp], lty=5)



### Task 2.2
nofComp <- 8



length(yVarTrain)
# trainPC <- xVars_pc$x[1:427, 1]
# testPC <- xVars_pc$x[428:527, 1]
# 
# k_accuracy <- caret::confusionMatrix( yVarTest,knn(train = trainPC, test = testPC,cl = yVarTrain, k = 30))$overall["Accuracy"]
error_lst <- c()
for (i in 1:8) {
  trainPC <- data.frame(xVars_pc$x[1:427, 1:i])
  testPC <- data.frame(xVars_pc$x[428:527, 1:i])
  k_accuracy <- caret::confusionMatrix( yVarTest,knn(train = trainPC, test = testPC,cl = yVarTrain, k = 30))$overall["Accuracy"]
  
  error_rate <- 1 - k_accuracy
  error_lst <- c(error_lst,error_rate)
}



plot( seq(1,8),error_lst,"l",xlab = "Number of Components", ylab = "Error Rates")

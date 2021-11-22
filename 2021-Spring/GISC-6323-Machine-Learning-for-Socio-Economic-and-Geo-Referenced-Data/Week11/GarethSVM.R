##
## Chapter 9 Lab: Support Vector Machines
##
rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

library(e1071); library(ISLR) ;library(gmodels); library(ROCR); library(caret)

##
## Support Vector Classifier
##
RNGversion("3.5.1"); set.seed(1)

x <- matrix(rnorm(20*2), ncol=2)
y <- c(rep(-1,10), rep(1,10))
x[y==1,] <- x[y==1,] + 1
plot(x[,1]~x[,2], col=(3-y), cex=1.5, pch=19)
df <- data.frame(x=x, y=as.factor(y))

help(svm)
svmfit <- svm(y~., data=df, kernel="linear", cost=10, scale=FALSE)
plot(svmfit, df)
svmfit$index           # Observations used as support vectors
summary(svmfit)

svmfit <- svm(y~., data=df, kernel="linear", cost=0.1, scale=FALSE)
plot(svmfit, df)
svmfit$index
summary(svmfit)

## Find best cost parameter
set.seed(1)
tune.out <- tune(svm, y~.,data=df,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
bestmod <- tune.out$best.model
summary(bestmod)

## Set up test data
xtest <- matrix(rnorm(20*2), ncol=2)
ytest <- sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,] <- xtest[ytest==1,] + 1
testdat <- data.frame(x=xtest, y=as.factor(ytest))
ypred <- predict(bestmod, testdat)
table(predict=ypred, truth=testdat$y)

## Change cost value
svmfit <- svm(y~., data=df, kernel="linear", cost=0.01, scale=FALSE)
ypred <- predict(svmfit,testdat)
table(predict=ypred, truth=testdat$y)

## Define linearly separable training sample by increasing the seperation
x[y==1,] <- x[y==1,]+0.5
plot(x[,1]~x[,2], col=(y+5)/2, cex=1.5, pch=19)
df <- data.frame(x=x, y=as.factor(y))
svmfit <- svm(y~., data=df, kernel="linear", cost=1e5)
summary(svmfit)
plot(svmfit, df)
svmfit=svm(y~., data=df, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit,df)

##
## Support Vector Machine
##
set.seed(1)
x <- matrix(rnorm(200*2), ncol=2)
x[1:100,] <- x[1:100,]+2
x[101:150,] <- x[101:150,]-2
y <- c(rep(1,150),rep(2,50))
df <- data.frame(x=x, y=as.factor(y))
plot(x[,1]~x[,2], col=y)
train <- sample(1:200, 100)
svmfit <- svm(y~., data=df[train,], kernel="radial",  gamma=1, cost=1)
plot(svmfit, df[train,])
summary(svmfit)
svmfit <- svm(y~., data=df[train,], kernel="radial",gamma=1, cost=1e5)
plot(svmfit,df[train,])
summary(svmfit)

set.seed(1)
tune.out <- tune(svm, y~., data=df[train,], kernel="radial", 
                 ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
summary(tune.out$best.model)
table(true=df[-train,"y"], 
      pred=predict(tune.out$best.model,newdata=df[-train,]))

# ROC Curves
rocplot <- function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)}

old.par <- par(mfrow=c(1,2))
## Training ROC
svmfit.opt <- svm(y~., data=df[train,], kernel="radial",gamma=2, cost=1, decision.values=T)
fitted <- attributes(predict(svmfit.opt, df[train,], decision.values=TRUE))$decision.values
rocplot(fitted,df[train,"y"], main="Training Data")
svmfit.flex <- svm(y~., data=df[train,], kernel="radial",gamma=50, cost=1, decision.values=T)
fitted <- attributes(predict(svmfit.flex,df[train,],decision.values=T))$decision.values
rocplot(fitted,df[train,"y"], add=T, col="red")
## Test ROC
fitted <- attributes(predict(svmfit.opt, df[-train,],decision.values=T))$decision.values
rocplot(fitted,df[-train,"y"], main="Test Data")
fitted <- attributes(predict(svmfit.flex, df[-train,],decision.values=T))$decision.values
rocplot(fitted, df[-train,"y"], add=T, col="red")
par(old.par)

# SVM with Multiple Classes
set.seed(1)
x <- rbind(x, matrix(rnorm(50*2), ncol=2))
y <- c(y, rep(0,50))
x[y==0,2] <- x[y==0,2]+2
df <- data.frame(x=x, y=as.factor(y))

plot(x[,1]~x[,2], col=(y+1))
svmfit <- svm(y~., data=df, kernel="radial", cost=10, gamma=1)
plot(svmfit, df)

##
## Application to Gene Expression Data
##
library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)
table(Khan$ytrain)
table(Khan$ytest)
df <- data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
## High dimensional data justify linear hyperplanes
out <- svm(y~., data=df, kernel="linear",cost=10)
summary(out)
table(out$fitted, df$y)
df.te <- data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te <- predict(out, newdata=df.te)
table(pred.te, df.te$y)

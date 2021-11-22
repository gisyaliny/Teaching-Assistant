# Chapter 8 Lab: Decision Trees
library(tree); library(randomForest); library(gbm); library(ISLR)
library(C50); library(gmodels); library(pROC); library(caret)

rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

# setwd("E:\\Lectures2019\\GISC6323\\Lecture07")
# RNGversion("3.5.2") 
summary(Carseats)

Carseats$High <- as.factor(ifelse(Carseats$Sales<=8,"no","yes"))
tree.carseats <- tree(High~.-Sales, data=Carseats, split="gini",
                      control=tree.control(nobs = nrow(Carseats),minsize=2, mincut=1))
summary(tree.carseats)
plot(tree.carseats); text(tree.carseats, pretty=0)
tree.carseats
salesProb <- predict(tree.carseats, type="vector")
salesProb
## Evaluate full Tree predictions
salesPredYes <- as.factor(ifelse(salesProb[,"yes"] <= 0.5, "no", "yes"))
caret::confusionMatrix(Carseats$High, salesPredYes, 
                       positive = "yes", dnn = c("Reference","Prediction"))

## Recall:
##   Sensitivity: Prob predict positive given true positive
##   Specificity: Prob predict negative given true negative
rocTree1 <- roc(Carseats$High, salesProb[,"yes"])
plot(rocTree1, main="ROC curve for Trees", 
     col="blue", lwd=2, legacy.axes=FALSE)
legend("bottomright", legend=c("Full Tree"), lty=1, col=c("blue"))

##
## Evaluate test sample
##
set.seed(123)
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train,]
High.test <- Carseats$High[-train]
tree.carseats <- tree(High~.-Sales, Carseats, subset=train)
salesProb <- predict(tree.carseats, Carseats.test, type="vector")
salesPredYes <- as.factor(ifelse(salesProb[,"yes"] <= 0.5, "no", "yes"))
caret::confusionMatrix(High.test, salesPredYes, 
                       positive = "yes", dnn = c("Reference","Prediction"))


## Pruning training tree
set.seed(123)
cv.carseats <- cv.tree(tree.carseats, FUN=prune.misclass, K=10)
names(cv.carseats)
cv.carseats
plot(cv.carseats$size, cv.carseats$dev, type="b", main="Deviance")
plot(cv.carseats$k, cv.carseats$dev, type="b", main="Complexity Parameter")

prune.carseats <- prune.misclass(tree.carseats, best=3)
plot(prune.carseats); text(prune.carseats, pretty=0)

salesProb <- predict(prune.carseats, Carseats.test, type="vector")
salesPredYes <- as.factor(ifelse(salesProb[,"yes"] <= 0.5, "no", "yes"))
caret::confusionMatrix(High.test, salesPredYes, 
                       positive = "yes", dnn = c("Reference","Prediction"))

##
## Fitting Regression Trees
##
library(MASS)
set.seed(123)
summary(Boston)
train <-  sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston <- tree(medv~., Boston, subset=train)
summary(tree.boston)
plot(tree.boston); text(tree.boston, pretty=0)
title(main="Boston Median Home Values")

cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev, type='b')
prune.boston <- prune.tree(tree.boston, best=4)
plot(prune.boston); text(prune.boston, pretty=0)
yhat <- predict(tree.boston, newdata=Boston[-train,])
boston.test <- Boston[-train,"medv"]
plot(yhat~boston.test); abline(0,1)
mean((yhat-boston.test)^2)

##
## Bagging
##
set.seed(123)
## Use all variables as potential variables mtry=13
## Out-off bag observations could be used for model evaluation
bag.boston <- randomForest(medv~., data=Boston, subset=train, 
                           mtry=13, importance=TRUE)
bag.boston
yhat.bag <-  predict(bag.boston, newdata=Boston[-train,])
plot(yhat.bag~boston.test); abline(0,1)
mean((yhat.bag-boston.test)^2)

## Set bootstrapping samples
bag.boston <- randomForest(medv~., data=Boston, subset=train, 
                           mtry=13, ntree=50)
yhat.bag <- predict(bag.boston, newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)

## Random forest
set.seed(123)
rf.boston <- randomForest(medv~., data=Boston, subset=train, 
                          mtry=6, importance=TRUE)
yhat.rf <- predict(rf.boston, newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)
importance(rf.boston)
varImpPlot(rf.boston)

# Gradient boosting
set.seed(123)
boost.boston <- gbm(medv~., data=Boston[train,], distribution="gaussian", 
                    n.trees=5000, interaction.depth=4)
summary(boost.boston)
plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")
yhat.boost <- predict(boost.boston, newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost-boston.test)^2)

boost.boston <- gbm(medv~., data=Boston[train,], distribution="gaussian", n.trees=5000, 
                    interaction.depth=4, shrinkage=0.2)
yhat.boost <- predict(boost.boston, newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost-boston.test)^2)

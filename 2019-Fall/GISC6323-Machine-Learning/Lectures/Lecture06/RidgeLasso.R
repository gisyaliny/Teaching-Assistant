##
## Chapter 6: Regularization with Ridge Regression and the Lasso
## see http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%206%20Labs.txt
##
rm(list=ls())
library(ISLR); library(glmnet)

RNGversion(vstr="3.5.1"); set.seed(1234)

help(Hitters)
View(Hitters)
Hitters <- na.omit(Hitters)                       # Remove observations with NAs

x <- model.matrix(Salary~., data=Hitters)[,-1]    # Design matrix without the intercept
y <- Hitters$Salary

##
## Ridge Regression. Note x variables are standardize by default in glmnet                            
##
grid <- 10^seq(10,-2,length=100)                  # search grid for lambda
help("glmnet")
ridge.mod <- glmnet(x, y, alpha=0, lambda=grid)   # alpha=0 => Ridge regression

dim(coef(ridge.mod))                              # 20 coefficients by 100 lambdas

ridge.mod$lambda[50]                              # lambda value
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))               # l2-norm

ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

## coefficients using the predict function allows for lambda-values not in grid
help(predict.glmnet)
predict(ridge.mod, s=50, type="coefficients")[1:20,]

## split df into train and test samples
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]
y.train <- y[train]

## Baseline lm model MSE
dfTrain <- data.frame(y.train, x[train, ])
dfTest <- data.frame(y.test, x[test, ])
lm.01 <- lm(y.train~., data=dfTrain)
summary(lm.01)
pred.lm <- predict(lm.01, newdata=dfTest)
mean((pred.lm-y.test)^2)                     # Baseline MSE

##
## Evaluate MSE on predicted test data for different lambdas
##
ridge.mod<- glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh=1e-12)
help("plot.glmnet")
plot(ridge.mod, xvar="lambda", label=TRUE)

## Basic lm model with lambda=0
ridge.pred <- predict(ridge.mod, s=0, exact=T, newx=x[test,], x=x[train,] ,y=y[train])
mean((ridge.pred-y.test)^2)

ridge.pred <- predict(ridge.mod, s=4, type="response", newx=x[test,])    # lambda=4
mean((ridge.pred-y.test)^2)                       # prediction MSE
mean((mean(y[train])-y.test)^2)                   # MSE if only intercept

ridge.pred <- predict(ridge.mod, s=1e10, type="response", newx=x[test,]) # lambda=1e10
mean((ridge.pred-y.test)^2)

##
## Find best lambda by n-fold cross evaluation
##
help("cv.glmnet")
cv.out <- cv.glmnet(x[train,], y[train], alpha=0) # procedure choosen sequence of lambdas
help("plot.cv.glmnet")
plot(cv.out)
(bestlam <- cv.out$lambda.min)
log(bestlam)

ridge.pred <- predict(ridge.mod, s=bestlam, type="response", newx=x[test,])
mean((ridge.pred-y.test)^2)

##
## The Lasso obtained with alpha=1
##
lasso.mod <- glmnet(x[train,], y[train], alpha=1)
plot(lasso.mod, xvar="lambda", label=TRUE)

cv.out <- cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)
(bestlam <- cv.out$lambda.min)

lasso.pred <- predict(lasso.mod, s=bestlam, type="response", newx=x[test,])
mean((lasso.pred-y.test)^2)
lasso.mod <- glmnet(x, y, alpha=1, lambda=grid)
lasso.coef <- predict(lasso.mod, type="coefficients", s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]



rm(list=ls())
library(ISLR); library(gmodels); library(MASS); library(class)
data("Smarket")


##
## High discrimination example in logistic regression
##
y <- c(0,0,0,0,0,1,1,1,1,1)
x <- c(1,2,3,4,5,6,7,8,9,10)
plot(y~x, type="b")
glmHiDis <- glm(y~x, family=binomial, control=list(epsilon=1e-15,
                                                   maxit=50, trace=TRUE))
summary(glmHiDis)

##
## Split data into a training and a test sample
##
SmarketTrain <- Smarket[Smarket$Year < 2005, ]
SmarketTest <- Smarket[Smarket$Year == 2005, ]

##
## Linear Discriminant Analysis
##
lda.fit <- lda(Direction~Lag1+Lag2,data=SmarketTrain)
lda.fit
plot(lda.fit)

##
## Predict in test sample
##
lda.pred <- predict(lda.fit, SmarketTest)
names(lda.pred)
lda.class <- lda.pred$class

##
## Evaluate model
##
CrossTable(x=SmarketTest$Direction, y=lda.class, prop.r=F, prop.c=F, prop.chisq = FALSE)
mean(lda.class==SmarketTest$Direction)
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)
data.frame(obs.class=lda.class[1:20], Pr.down=lda.pred$posterior[1:20,1],
           Pr.up=lda.pred$posterior[1:20,2])

sum(lda.pred$posterior[,1] > 0.9)

##
## Quadratic Discriminant Analysis
##
qda.fit <- qda(Direction~Lag1+Lag2,data=SmarketTrain)
qda.fit
qda.class <- predict(qda.fit, SmarketTest)$class
CrossTable(x=SmarketTest$Direction, y=qda.class, prop.r=F, prop.c=F, prop.chisq = FALSE)
mean(qda.class==SmarketTest$Direction)

##
## For comparison: K-Nearest Neighbors
##
set.seed(1)
knn.pred <- knn(SmarketTrain[, 2:3],SmarketTest[, 2:3],SmarketTrain$Direction,k=1)
CrossTable(x=SmarketTest$Direction, y=knn.pred, prop.r=F, prop.c=F, prop.chisq = FALSE)
(83+43)/252

knn.pred <- knn(SmarketTrain[, 2:3],SmarketTest[, 2:3],SmarketTrain$Direction,k=3)
CrossTable(x=SmarketTest$Direction, y=knn.pred, prop.r=F, prop.c=F, prop.chisq = FALSE)
mean(knn.pred==SmarketTest$Direction)


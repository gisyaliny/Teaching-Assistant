rm(list=ls())
library(ISLR); library(gmodels)
data("Smarket")
help(Smarket)

## Descriptive Exploration
summary(Smarket)
round(cor(Smarket[-9]), 2)
plot(Smarket$Volume, xlab="Time Index", main="Trade Volume Over Time")
plot(Volume~Year, data=Smarket)
plot(Today~Direction, data=Smarket)

##
## Start glm analysis
##
contrasts(Smarket$Direction)
dir.Stock <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket,
               family=binomial(link="logit"))
summary(dir.Stock)
plot(effects::allEffects(dir.Stock))

## Predict up-swing probabilities
prob.Stock <- predict(dir.Stock, type="response")
head(prob.Stock, n=10)
hist(prob.Stock); abline(v=0.5, col="red")
dir.Pred <- ifelse(prob.Stock < 0.5, "Down", "Up")

## Evaluate Prediction
CrossTable(x = Smarket$Direction, y = dir.Pred, prop.r=F, prop.c=F, prop.chisq = FALSE)
(145+507)/1250
mean(dir.Pred==Smarket$Direction)

##
## Split data into a training and a test sample
##
SmarketTrain <- Smarket[Smarket$Year < 2005, ]
SmarketTest <- Smarket[Smarket$Year == 2005, ]

dir.Train <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=SmarketTrain,
                 family=binomial(link="logit"))
prob.StockTest <- predict(dir.Train, newdata=SmarketTest, type="response")
dir.Pred <- ifelse(prob.StockTest < 0.5, "Down", "Up")
CrossTable(x=SmarketTest$Direction, y=dir.Pred, prop.r=F, prop.c=F, prop.chisq = FALSE)
(77+44)/252

## Revised model with less overfitting
dir.Train <- glm(Direction~Lag1+Lag2, data=SmarketTrain,
                family=binomial(link="logit"))
summary(dir.Train)
prob.StockTest <- predict(dir.Train, newdata=SmarketTest, type="response")
dir.Pred <- ifelse(prob.StockTest < 0.5, "Down", "Up")
CrossTable(x=SmarketTest$Direction, y=dir.Pred, prop.r=F, prop.c=F, prop.chisq = FALSE)
(35+106)/252

## Evaluate scenarios of streaks of up or down turns
predict(dir.Train, newdata=data.frame(Lag1=c(2,-2),Lag2=c(2,-2)), type="response")

##
## Exercise: use kNN to predict probabilities and add these probablities as an
##           independent variable to the logistic regression model.
##           [a] Will the model increase? 
##           [b] How do the logistic and kNN models compare?
##
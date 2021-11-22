##### Regression Tree Methods -------------------
library(Cubist); library(rpart); library(rpart.plot)
library(gmodels); library(pROC); library(caret)
rm(list=ls())
setwd("E:\\Lectures2019\\GISC6323\\Lecture07")
RNGversion("3.5.2")

## Example: Estimating Wine Quality ----
## Exploring and preparing the data ----
wine <- read.csv("whitewines.csv")
summary(wine)

# the distribution of quality ratings
hist(wine$quality)

wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]

## Training a model on the data ----
# regression tree using rpart
m.rpart <- rpart(quality ~ ., data = wine_train)

# get basic information about the tree
m.rpart

# get more detailed information about the tree
summary(m.rpart)

# a basic decision tree diagram
rpart.plot(m.rpart, digits = 3)

# a few adjustments to the diagram
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

## Evaluate model performance ----

# generate predictions for the testing dataset
p.rpart <- predict(m.rpart, wine_test)

# compare the distribution of predicted values vs. actual values
summary(p.rpart)
summary(wine_test$quality)

# compare the correlation
cor(p.rpart, wine_test$quality)

# function to calculate the mean absolute error
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))  
}

# mean absolute error between predicted and actual values
MAE(p.rpart, wine_test$quality)

# mean absolute error between actual values and mean value
mean(wine_train$quality) # result = 5.87
MAE(5.87, wine_test$quality)

## Step 5: Improving model performance ----
# train a Cubist Model Tree
m.cubist <- cubist(x = wine_train[-12], y = wine_train$quality)

# display basic information about the model tree
m.cubist

# display the tree itself
summary(m.cubist)
plot(m.cubist)

# generate predictions for the model
p.cubist <- predict(m.cubist, wine_test)

# summary statistics about the predictions
summary(p.cubist)

# correlation between the predicted and true values
cor(p.cubist, wine_test$quality)

# mean absolute error of predicted and true values
# (uses a custom function defined above)
MAE(wine_test$quality, p.cubist)


library(dplyr); library(ggplot2); library(gmodels)  
## Modeling process packages
library(caret)     # for resampling and model training
library(rsample)   # splitting the data into training and test datasets

#Import Credit dataset
credit <- read.csv("credit.csv",header = TRUE, stringsAsFactors = TRUE)
sapply(credit, is.factor)

#Data splitting
set.seed(123)
index <- sample(1:nrow(credit), round(nrow(credit) * 0.75))
train <-credit[index,]
test <- credit[-index,]

#Resampling
cv <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5
)

#Create grid of hyperparameter values
hyper_grid <- expand.grid (k = seq(2, 25, by = 1))

#Knn model using grid search
knn_fit <- train(
  credit ~.,
  data = train,
  method = "knn",
  trControl = cv,
  tuneGrid = hyper_grid,
  metric = "Accuracy"
)

#Print and plot CV results
knn_fit
ggplot(knn_fit)

#Evaluate predictive quality of model
defaultPred <- predict(knn_fit, test, type = "prob")
plot(defaultPred$yes~default, data = test)
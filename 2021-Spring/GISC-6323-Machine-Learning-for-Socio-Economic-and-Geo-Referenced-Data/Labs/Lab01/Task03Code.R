rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

## Data manipulation and visualization
library(dplyr); library(ggplot2); library(gmodels)       

## Modeling process packages
library(caret)     # for resampling and model training
library(rsample)   # splitting the data into training and test datasets

credit <-read.csv("credit.csv",
                    header = TRUE, stringsAsFactors = TRUE)
sapply(credit, is.factor)

# Stratified sampling with the rsample package
set.seed(123)
split <- initial_split(credit, prop = 0.7, 
                       strata = "default")
default_train  <- training(split)
default_test   <- testing(split)
# Specify resampling strategy
cv <- trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 5
)

# Create grid of hyperparameter values
hyper_grid <- expand.grid(k = seq(30, 80, by = 1))

# Tune a knn model using grid search
knn_fit <- train(
  default ~ . , 
  data = default_train, 
  method = "knn", 
  trControl = cv, 
  tuneGrid = hyper_grid,
  metric = "Accuracy" 
)

knn_fit$bestTune
knn_fit
ggplot(knn_fit)

defaultPred <- predict(knn_fit, default_test, type="prob")
plot(defaultPred$yes~default, data=default_test); abline(h=0.5, col="red")
plot(default~defaultPred$yes, data=default_test)

##
## Logistic Regression
##
creBase <- glm(default~1 , data=default_train, family=binomial)
creStep <- step(creBase, scope=~ checking_balance + months_loan_duration + credit_history + 
                  purpose + amount + savings_balance + employment_duration + 
                  percent_of_income + years_at_residence + age + other_credit + 
                  housing + existing_loans_count + job + dependents + phone,
                direction="forward")
summary(creStep)
predGLM <- predict(creStep, newdata=default_test, type="response")
plot(predGLM~default, data=default_test); abline(h=0.5, col="red")
plot(default~predGLM, data=default_test)


rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

## Data manipulation and visualization
library(dplyr); library(ggplot2); library(gmodels)       

## Modeling process packages
library(caret)     # for resampling and model training
library(pROC)      # for ROC analysis
library(rsample)   # splitting the data into training and test datasets

credit <-read.csv("credit.csv",header = TRUE, stringsAsFactors = TRUE)
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

## Calculate predicted default probabilities
summary(credit$default)
predkNN <- predict(knn_fit, default_test, type="prob")
predGLM <- predict(creStep, newdata=default_test, type="response")
hist(predkNN[,"yes"])
hist(predGLM)

delta <- 0.4                  # Set cut-off probability
## Convert probabilities with cut-off to default and no-default
kNNPredDefault <- ifelse(predkNN[,"yes"] <= delta, "no", "yes")
kNNPredDefault <- as.factor(kNNPredDefault)
barchart(kNNPredDefault)
## Evaluate kNN predictions
CrossTable(default_test$default, kNNPredDefault)
caret::confusionMatrix(default_test$default, kNNPredDefault, 
                       positive = "yes",dnn = c("Reference","Prediction"))
?caret::confusionMatrix

## Convert probabilities with cut-off to default and no-default
glmPredDefault <- ifelse(predGLM <= delta, "yes", "no")
glmPredDefault <- as.factor(glmPredDefault)
barchart(glmPredDefault)
## Evaluate kNN predictions
CrossTable(default_test$default, glmPredDefault)

caret::confusionMatrix(default_test$default, glmPredDefault, 
                       positive = "yes",dnn = c("Reference","Prediction"))

## Evaluate ROC curves for kNN
rockNN <- roc(default_test$default, predkNN[,"yes"])
plot(rockNN, main="ROC curve for kNN and GLM predicted probabilities", 
     col="blue", lwd=2, legacy.axes=TRUE)
legend("bottomright", legend=c("kNN","GLM"), lty=1, col=c("blue","red"))

# compare GLM 
rocGLM <- roc(default_test$default, predGLM)
plot(rocGLM, col="red", lwd=2, add=TRUE)

# calculate AUC for Naive Bayes and kNN
auc(rockNN)
auc(rocGLM)


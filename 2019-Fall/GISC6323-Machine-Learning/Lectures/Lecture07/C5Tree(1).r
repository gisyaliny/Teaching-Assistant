library(C50); library(gmodels); library(pROC); library(caret)
rm(list=ls())
setwd("E:\\Lectures2019\\GISC6323\\Lecture07")
RNGversion("3.5.2")

#### Decision Trees

## Example: Identifying Risky Bank Loans ----
## Exploring and preparing the data ----
credit <- read.csv("credit.csv")
summary(credit)


# look at the class variable
table(credit$default)

# create a random sample for training and test data
# use set.seed to use the same random number sequence as the tutorial
set.seed(123)
train_sample <- sample(1000, 900)

# split the data frames
credit_train <- credit[train_sample, ]
credit_test  <- credit[-train_sample, ]

# check the proportion of class variable
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

## Training a model on the data ----
# build the simplest decision tree
C5.0Control(winnow = FALSE)
credit_model <- C5.0(default~. , data=credit_train, rules=FALSE, trials=1)

# display simple facts about the tree
credit_model

# display detailed information about the tree
summary(credit_model)

## Evaluating model performance ----
# create a factor vector of predictions on test data
# for type="prob" wrap result in as.data.frame()
credit_pred <- predict(credit_model, credit_test, type="class")

# cross tabulation of predicted versus actual classes
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = TRUE, prop.t = FALSE,
           dnn = c('actual default', 'predicted default'))

## Improving model performance ----

## Bagging the accuracy of decision trees
# bagging decision tree with 10 trials
credit_boost10 <- C5.0(default~. , data=credit_train, trials = 10)
credit_boost10
summary(credit_boost10)

credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = TRUE, prop.t = FALSE,
           dnn = c('actual default', 'predicted default'))

## Making some mistakes more costly than others

# create dimensions for a cost matrix
matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("predicted", "actual")
matrix_dimensions

# build the matrix
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2, dimnames = matrix_dimensions)
error_cost

# apply the cost matrix to the tree
credit_cost <- C5.0(default~. , data=credit_train, costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)

CrossTable(credit_test$default, credit_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = TRUE, prop.t = FALSE,
           dnn = c('actual default', 'predicted default'))
##
## Outlook: Exploring combinations of hyper-parameters
##          caret::train
##
caret::modelLookup()
modelLookup("C5.0")

## Training with different combinations of parameters
m <- train(default~. , data=credit_train, method="C5.0")
m
credit_select_pred <- predict(m, credit_test)
CrossTable(credit_test$default, credit_select_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = TRUE, prop.t = FALSE,
           dnn = c('actual default', 'predicted default'))

## Setting search hyper-parameters
ctrl <- trainControl(method="cv", number=10,          # 10-fold CV
                     selectionFunction="best")
grid <- expand.grid(model="tree", 
                    trials=c(1,5,10,20,25,30,35),
                    winnow=FALSE)
grid
m <- train(default~., data=credit, method="C5.0",
           metric="Kappa",
           trControl=ctrl,
           tuneGrid=grid)
m

rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

# Helper packages
library(dplyr)       # for data wrangling
library(ggplot2)     # for awesome plotting

# Modeling packages
library(rpart)       # direct engine for decision tree application
library(caret)       # meta engine for decision tree application

# Model interpretability packages
library(rpart.plot)  # for plotting decision trees
library(vip)         # for feature importance
library(pdp)         # for feature effects

## Data sets
library(AmesHousing)
ames <- make_ames()

##
## Simple random sampling with caret
##
set.seed(123)  # for reproducibility
index <- createDataPartition(ames$Sale_Price, p = 0.7, list = FALSE)
train <- ames[index, ]
test  <- ames[-index, ]

##
## CART regression
##
ames_dt1 <- rpart(
  formula = Sale_Price ~ .,
  data    = train,
  method  = "anova"
)
ames_dt1
rpart.plot(ames_dt1)
plotcp(ames_dt1)
ames_dt1$cptable


ames_dt2 <- rpart(
  formula = Sale_Price ~ .,
  data    = train,
  method  = "anova", 
  control = list(cp = 0, xval = 10)
)

plotcp(ames_dt2)
abline(v = 11, lty = "dashed")

## caret cross validation results
ames_dt3 <- train(
  Sale_Price ~ .,
  data = train,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 20
)
ggplot(ames_dt3)

## Feature Interpretation
vip(ames_dt3, num_features = 40, bar = FALSE)

## Construct partial dependence plots
p1 <- partial(ames_dt3, pred.var = "Gr_Liv_Area") %>% autoplot()
p2 <- partial(ames_dt3 , pred.var = "Year_Built") %>% autoplot()

# Display plots side by side
gridExtra::grid.arrange(p1, p2, ncol = 2)

## 
## Bagging
##
library(doParallel)  # for parallel backend to foreach
library(foreach)     # for parallel processing with for loops

# Modeling packages
library(caret)       # for general model fitting
library(rpart)       # for fitting decision trees
library(ipred)       # for fitting bagged decision trees
set.seed(123)

# train bagged model
ames_bag1 <- bagging(
  formula = Sale_Price ~ .,
  data = train,
  nbagg = 100,  
  coob = TRUE,
  control = rpart.control(minsplit = 2, cp = 0)
)
ames_bag1
## Note: vip::vip available in caret::train but not in bagging, but bagging faster
## Using rpart in a parallel environment
cl <- makeCluster(8) # use 8 workers
registerDoParallel(cl) # register the parallel backend

# Fit trees in parallel and compute predictions on the test set
predictions <- foreach(
  icount(100), 
  .packages = "rpart", 
  .combine = cbind) %dopar% {
  # bootstrap copy of training data
  index <- sample(nrow(train), replace = TRUE)
  train_boot <- train[index, ]  
  
  # fit tree to bootstrap copy
  bagged_tree <- rpart(
    Sale_Price ~ ., 
    control = rpart.control(minsplit = 2, cp = 0),
    data = train_boot
  ) 
  
  predict(bagged_tree, newdata = test)
}
# Shutdown parallel cluster
stopCluster(cl)

## Visualize RMSE
predictions %>%
  as.data.frame() %>%
  mutate(
    observation = 1:n(),
    actual = test$Sale_Price) %>%
  tidyr::gather(tree, predicted, -c(observation, actual)) %>%
  group_by(observation) %>%
  mutate(tree = stringr::str_extract(tree, '\\d+') %>% as.numeric()) %>%
  ungroup() %>%
  arrange(observation, tree) %>%
  group_by(observation) %>%
  mutate(avg_prediction = cummean(predicted)) %>%
  group_by(tree) %>%
  summarize(RMSE = RMSE(avg_prediction, actual)) %>%
  ggplot(aes(tree, RMSE)) +
  geom_line() +
  xlab('Number of trees')

##
## Random Forest
##
library(ranger)    ## It is internally parallel
# number of features
n_features <- length(setdiff(names(train), "Sale_Price"))
n_features

# train a default random forest model
ames_rf1 <- ranger(
  Sale_Price ~ ., 
  data = train,
  mtry = floor(n_features / 3),
  respect.unordered.factors = "order",
  seed = 123
)

# get OOB RMSE
(default_rmse <- sqrt(ames_rf1$prediction.error))

# create hyperparameter grid
hyper_grid <- expand.grid(
  mtry = floor(n_features * c(.05, .15, .25, .333, .4)),
  min.node.size = c(1, 3, 5, 10), 
  replace = c(TRUE, FALSE),                               
  sample.fraction = c(.5, .63, .8),                       
  rmse = NA                                               
)

## execute full cartesian grid search
for(i in seq_len(nrow(hyper_grid))) {
  # fit model for ith hyperparameter combination
  fit <- ranger(
    formula         = Sale_Price ~ ., 
    data            = train, 
    num.trees       = n_features * 10,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$min.node.size[i],
    replace         = hyper_grid$replace[i],
    sample.fraction = hyper_grid$sample.fraction[i],
    verbose         = FALSE,
    seed            = 123,
    respect.unordered.factors = 'order',
  )
  # export OOB error 
  hyper_grid$rmse[i] <- sqrt(fit$prediction.error)
}

## assess top 10 models
hyper_grid %>%
  arrange(rmse) %>%
  mutate(perc_gain = (default_rmse - rmse) / default_rmse * 100) %>%
  head(10)

## re-run model with impurity-based variable importance
rf_impurity <- ranger(
  formula = Sale_Price ~ ., 
  data = train, 
  num.trees = 2000,
  mtry = 32,
  min.node.size = 1,
  sample.fraction = .80,
  replace = FALSE,
  importance = "impurity",
  respect.unordered.factors = "order",
  verbose = FALSE,
  seed  = 123
)

# re-run model with permutation-based variable importance
rf_permutation <- ranger(
  formula = Sale_Price ~ ., 
  data = train, 
  num.trees = 2000,
  mtry = 32,
  min.node.size = 1,
  sample.fraction = .80,
  replace = FALSE,
  importance = "permutation",
  respect.unordered.factors = "order",
  verbose = FALSE,
  seed  = 123
)
p1 <- vip::vip(rf_impurity, num_features = 25, bar = FALSE)
p2 <- vip::vip(rf_permutation, num_features = 25, bar = FALSE)

gridExtra::grid.arrange(p1, p2, nrow = 1)

##
## Gradient Boosting
##
library(gbm)
# run a basic GBM model
set.seed(123)  # for reproducibility
ames_gbm1 <- gbm(
  formula = Sale_Price ~ .,
  data = train,
  distribution = "gaussian",  # SSE loss function
  n.trees = 5000,
  shrinkage = 0.1,
  interaction.depth = 3,
  n.minobsinnode = 10,
  cv.folds = 10
)

## find index for number trees with minimum CV error
best <- which.min(ames_gbm1$cv.error)

## get MSE and compute RMSE
sqrt(ames_gbm1$cv.error[best])

## plot error curve
gbm.perf(ames_gbm1, method = "cv")

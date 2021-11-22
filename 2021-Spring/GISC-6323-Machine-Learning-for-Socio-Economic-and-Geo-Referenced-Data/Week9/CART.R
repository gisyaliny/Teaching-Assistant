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
p2 <- partial(ames_dt3, pred.var = "Year_Built") %>% autoplot()

# Display plots side by side
gridExtra::grid.arrange(p1, p2, ncol = 2)

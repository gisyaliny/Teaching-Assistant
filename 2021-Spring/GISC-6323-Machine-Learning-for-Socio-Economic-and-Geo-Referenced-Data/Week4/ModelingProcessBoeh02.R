rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

## Data manipulation and visualization
library(dplyr); library(ggplot2)       

## Data sets
library(AmesHousing)
data("attrition", package = "modeldata")       # Job attrition data

## Modeling process packages
library(rsample)   # for resampling procedures
library(caret)     # for resampling and model training
library(h2o)       # for resampling and model training

# h2o set-up 
h2o.no_progress()  # turn off h2o progress bars
h2o.init()         # launch h2o

## Setup data. No ordinal factors
ames <- AmesHousing::make_ames()  # Ames housing data
ames.h2o <- as.h2o(ames)          # convert to h2o data set

churn <-  attrition %>%                                    
  mutate_if(is.ordered, .funs = factor, ordered = FALSE)  # Recode ordinal factors
churn.h2o <- as.h2o(churn)

##
## Simple random sampling
##
# Using base R
set.seed(123)  # for reproducibility
index_1 <- sample(1:nrow(ames), round(nrow(ames) * 0.7))
train_1 <- ames[index_1, ]
test_1  <- ames[-index_1, ]

# Using caret package
set.seed(123)  # for reproducibility
index_2 <- createDataPartition(ames$Sale_Price, p = 0.7, 
                               list = FALSE)
train_2 <- ames[index_2, ]
test_2  <- ames[-index_2, ]

x <- c("Year_Sold","Gr_Liv_Area","Lot_Area")
y <- "Sale_Price"
h2o.cv <- h2o.glm(
  x = x, 
  y = y, 
  training_frame = ames.h2o,
  nfolds = 10  # perform 10-fold CV
)

# Using rsample package
set.seed(123)  # for reproducibility
split_1  <- initial_split(ames, prop = 0.7)
train_3  <- training(split_1)
test_3   <- testing(split_1)

# Using h2o package
split_2 <- h2o.splitFrame(ames.h2o, ratios = 0.7, 
                          seed = 123)
train_4 <- split_2[[1]]
test_4  <- split_2[[2]]

##
## Stratified random sampling
##
# orginal response distribution
table(churn$Attrition) %>% prop.table()

# stratified sampling with the rsample package
set.seed(123)
split_strat  <- initial_split(churn, prop = 0.7, 
                              strata = "Attrition")
train_strat  <- training(split_strat)
test_strat   <- testing(split_strat)

# consistent response ratio between train & test
table(train_strat$Attrition) %>% prop.table()
table(test_strat$Attrition) %>% prop.table()

?caret::trainControl  # see sampling option

##
## Resampling
##
x <- c("Year_Sold","Gr_Liv_Area","Lot_Area")
y <- "Sale_Price"
h2o.cv <- h2o.glm(
  x = x, 
  y = y, 
  training_frame = train_4,
  nfolds = 10  # perform 10-fold CV
)

vfold_cv(train_3, v=10)

##
## Bootstrapping
##
bootstraps(train_3, times=10)


##
## Putting things together
##

# Stratified sampling with the rsample package
set.seed(123)
split <- initial_split(ames, prop = 0.7, 
                       strata = "Sale_Price",
                       breaks=10)
ames_train  <- training(split)
ames_test   <- testing(split)
# Specify resampling strategy
cv <- trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 5
)

# Create grid of hyperparameter values
hyper_grid <- expand.grid(k = seq(2, 25, by = 1))

# Tune a knn model using grid search
knn_fit <- train(
  Sale_Price ~ Year_Sold+Gr_Liv_Area+Lot_Area+Neighborhood+Full_Bath, 
  data = ames_train, 
  method = "knn", 
  trControl = cv, 
  tuneGrid = hyper_grid,
  metric = "RMSE" 
)

knn_fit$bestTune
knn_fit
ggplot(knn_fit)

amesPred <- predict(knn_fit, ames_test)
plot(amesPred~Sale_Price, data=ames_test)
abline(a=0, b=1, col="red")
cor(amesPred, ames_test$Sale_Price)^2   # R-square



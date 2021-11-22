#######################################################################
## Principal component regression and Partial Least Squares Regression
## See Boehmke & Greenwell pp 96-104
## See Gareth James et al. pp 228-238 and pp 256-259 
## Standalone library pls with the function pcr and plsr
#######################################################################

rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

## Data manipulation and visualization
library(dplyr); library(ggplot2); library(caret); library(vip); library(pdp)      

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
## Principal component regressions with cross-validation
##
set.seed(123)  # for reproducibility
cv_model_pcr <- train(
  Sale_Price ~ ., 
  data = train, 
  method = "pcr",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 100
)

## model with lowest RMSE
cv_model_pcr$bestTune

## results for model with lowest RMSE
cv_model_pcr$results %>%
  dplyr::filter(ncomp == pull(cv_model_pcr$bestTune))

##
## Partial least squares with cross-validation
##
set.seed(123)  # for reproducibility
cv_model_pls <- train(
  Sale_Price ~ ., 
  data = train, 
  method = "pls",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 30
)
## model with lowest RMSE
cv_model_pls$bestTune

# results for model with lowest RMSE
cv_model_pls$results %>%
  dplyr::filter(ncomp == pull(cv_model_pls$bestTune))

## plot cross-validated RMSE
ggplot(cv_model_pls)

##
## Feature interpretation
##
vip(cv_model_pls, num_features = 20, method = "model")

partial(cv_model_pls, "Gr_Liv_Area", grid.resolution = 20, ylim=c(100000,300000), plot = TRUE)
partial(cv_model_pls, "First_Flr_SF", grid.resolution = 20, ylim=c(100000,300000)), plot = TRUE)
partial(cv_model_pls, "Garage_Cars", grid.resolution = 20, ylim=c(100000,300000), plot = TRUE)
partial(cv_model_pls, "Total_Bsmt_SF", grid.resolution = 20, ylim=c(100000,300000), plot = TRUE)



##
## Artificial Neural Networks 
##
rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

#install.packages("neuralnet")
RNGversion("3.5.3"); set.seed(12345) # to guarantee repeatable results
# setwd("E:\\Lectures2021\\GISC6323\\Week12")
library(neuralnet)
help(neuralnet)

concrete <- read.csv("concrete.csv")
summary(concrete)

# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# apply normalization to entire data frame
concrete_norm <- as.data.frame(lapply(concrete, normalize))

# create training and test data
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

# create a custom softplus activation function. Similar to rectified linear unit function
softplus <- function(x) { log(1 + exp(x)) }
curve(softplus, from=-5, to=5); abline(v=0, h=0, lty=3)

set.seed(12345) # to guarantee repeatable results
concrete_model3 <- neuralnet(strength ~ cement + slag + ash + water + superplastic + 
                                        coarseagg + fineagg + age, data = concrete_train, 
                             threshold = 0.01, stepmax=1000000,
                             linear.output = TRUE, hidden = c(5), act.fct = softplus)

# plot the network
plot(concrete_model3)

# evaluate the results as we did before
model_results3 <- predict(concrete_model3, concrete_test[1:8])
plot(model_results3~concrete_test$strength)
cor(model_results3, concrete_test$strength)

# note that the predicted and actual values are on different scales
strengths <- data.frame(
  actual = concrete$strength[774:1030],
  pred = model_results3
)

# create an unnormalize function to reverse the normalization
unnormalize <- function(x) { 
  return((x * (max(concrete$strength)) -
            min(concrete$strength)) + min(concrete$strength))
}

strengths$pred_new <- unnormalize(strengths$pred)
strengths$error <- strengths$pred_new - strengths$actual

head(strengths, n = 6)
plot(strengths$pred_new~strengths$actual)
cor(strengths$pred_new, strengths$actual)
hist(strengths$error/strengths$actual)  # Don't trust the predictions to build bridges

##
## K-fold cross-validation for small sample sizes
## 
set.seed(12345) # to guarantee repeatable results
K <- 5                                     # number of folds
idx <- sample(1:nrow(concrete))
folds <- cut(idx, breaks=K, labels=FALSE)
nofNeu <- 5                                # experiment with number of Neurons
scoreCor <- c()
system.time(
for (i in 1:K){
  cat("processing fold #", i, "\n")
  valIdx <- which(folds == i, arr.ind=TRUE)
  conVal <- concrete_norm[valIdx, ]
  conTrain <- concrete_norm[-valIdx, ]
  set.seed(12345) # to guarantee repeatable results
  modNN <- neuralnet(strength ~ ., data = conTrain,  linear.output = TRUE,
                     stepmax=1000000, threshold = 0.05,
                     hidden = nofNeu, act.fct = softplus)
  res <- predict(modNN, conVal[1:8])
  scoreCor <- c(scoreCor,cor(res, conVal$strength))
} #end::for
) #end::system.time
scoreCor  
mean(scoreCor)

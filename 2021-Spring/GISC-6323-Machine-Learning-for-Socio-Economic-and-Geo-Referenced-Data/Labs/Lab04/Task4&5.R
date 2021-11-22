##
## Lab04 Task4
##
rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

#install.packages("neuralnet")
RNGversion("3.5.3"); set.seed(12345)
library(neuralnet)
help("neuralnet")

## custom normalization function
normalize <- function(x) { (x - min(x)) / (max(x) - min(x)) }

## create the custom soft-plus activation function
softplus <- function(x) { log(1 + exp(x)) }

data(Boston, package="MASS")
summary(Boston)

# ## Exclude home values that were trunctated at $50k
# sel <- Boston$medv == 50
# table(sel)
# Boston <- Boston[!sel,]

## apply normalization to entire data frame
nBoston <- as.data.frame(lapply(Boston, normalize))

##
## Investigation of model structure without split into training and test sample
##

## Logistic Regression possible here because y transformed into (0,1). 
## This is just for demonstration purposes. It should NOT be done in practice!
## Significance not relevant here
logit01 <- glm(medv ~ ., data = nBoston, family=binomial())
summary(logit01)
car::vif(logit01)
logit01.pred <- predict(logit01, type="response")
cor(logit01.pred, nBoston$medv)

## Neural Network Model
nnet01 <- neuralnet(medv ~ ., data = nBoston, act.fct="logistic",
                    linear.output = TRUE, stepmax=10000, hidden = c(1))

## plot the network with weights and bias terms
plot(nnet01)

##
## Compare logistic parameters to NN weights. 
## Note intercept and bias coefficients are different.
##
str(nnet01)
data.frame(Logistic=coef(logit01), NeuroNet=nnet01$weights[[1]][[1]])
plot(coef(logit01)~nnet01$weights[[1]][[1]], 
     xlab="Neural Network", ylab="Logistic", 
     main="NN Weights against Logistic Coefficients")
text(nnet01$weights[[1]][[1]], coef(logit01), names(coef(logit01)))
abline(h=0, v=0, lty=3)                       # Reference frame
abline(a=0, b=1, lty=5)                       # Identity line

## evaluate the prediction
nnet01.pred <- predict(nnet01, nBoston[,1:13])
plot(nnet01.pred~nBoston$medv)
cor(nnet01.pred,nBoston$medv)

## back to original scale
medv.pred <- nnet01.pred * (max(Boston$medv)-min(Boston$medv)) + min(Boston$medv)
medv.error <- Boston$medv - medv.pred
hist(medv.error/Boston$medv)  # Error between -120% and 50% of the original median value

##
## Lab04 Task 5
##

## K-fold cross-validation needed because of small sample size.
## Best done in MS Open R, because of speed-up using parallel matrix calculations.
## Alternative use parallel "foreach" for simultaneous calculations
## May take in excess of an 1 hour

set.seed(12345) # to guarantee repeatable results
K <- 4                                            # number of folds
idx <- sample(1:nrow(nBoston))
folds <- cut(idx, breaks=K, labels=FALSE)
nOfNeurons <- 4:6                                # value range of neuron numbers in first layer
scoreCor <- matrix(0, nrow=length(nOfNeurons), ncol=K)

system.time(
  for (j in 1:length(nOfNeurons)){
    for (i in 1:K){
      cat("processing neurons=",nOfNeurons[j],"for fold #", i, "\n")
      valIdx <- which(folds == i, arr.ind=TRUE)
      test <- nBoston[valIdx, ]
      train <- nBoston[-valIdx, ]
      set.seed(12345) # to guarantee repeatable results
      medvNN <- neuralnet(medv ~ ., data = train,  linear.output = TRUE,
                         stepmax=10000000, hidden = nOfNeurons[j], act.fct = softplus)
      result <- predict(medvNN, test[1:13])
      scoreCor[j,i] <- cor(result, test$medv)
    } #end::for_i
  } #end::for_j
) #end::system.time
colnames(scoreCor) <- c("Fold 1", "Fold 2", "Fold 3", "Fold 4")
result <- data.frame(nOfNeurons, Average=rowMeans(scoreCor), scoreCor)

## Evaluate fit by number of nodes in a one-layer neural network.
round(result,3)             


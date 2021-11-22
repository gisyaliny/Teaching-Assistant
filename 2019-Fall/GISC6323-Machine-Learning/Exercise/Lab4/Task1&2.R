## Lab04 Task1

#install.packages("neuralnet")
rm(list=ls()); RNGversion("3.5.3"); set.seed(12345)
library(neuralnet)

## custom normalization function
normalize <- function(x) { return((x - min(x)) / (max(x) - min(x)))}

## create a custom softplus activation function
softplus <- function(x) { log(1 + exp(x)) }

data(Boston, package="MASS")
summary(Boston)

# ## Exclude home vales that were trunctated at $50k
# sel <- Boston$medv == 50
# table(sel)
# Boston <- Boston[!sel,]

## apply normalization to entire data frame
nBoston <- as.data.frame(lapply(Boston, normalize))

##
## Investigation of model structure without split into training and test sample
##

## Logistic Regression Model
logit01 <- glm(medv ~ ., data = nBoston, family=binomial())
summary(logit01)
car::vif(logit01)
logit01.pred <- predict(logit01, type="response")
cor(logit01.pred, nBoston$medv)

## Neural Network Model
nnet01 <- neuralnet(medv ~ ., data = nBoston, act.fct="logistic",
                    linear.output = TRUE, stepmax=1e+6, hidden = c(1))

## plot the network
plot(nnet01)

## Compare logistic parameters to NN weights
cbind(logisitc=coef(logit01), neuronet=nnet01$weights[[1]][[1]])
plot(coef(logit01)~nnet01$weights[[1]][[1]], 
     xlab="Neural Network", ylab="Logistic")
abline(h=0,v=0, lty=5)

## evaluate the prediction
nnet01.pred <- predict(nnet01, nBoston[,1:13])
plot(nnet01.pred~nBoston$medv)
cor(nnet01.pred,nBoston$medv)

## back to original scale
medv.pred <- nnet01.pred * (max(Boston$medv)-min(Boston$medv)) + min(Boston$medv)
medv.error <- Boston$medv - medv.pred
hist(medv.error/Boston$medv)  # Error between -120% and 50% of the original median value

##
## Lab04 Task 2
##

## K-fold cross-validation for small sample sizes.
##  Best done in MS Open R because of parallel matrix calculation capabilities

set.seed(12345) # to guarantee repeatable results
K <- 5                                            # number of folds
idx <- sample(1:nrow(nBoston))
folds <- cut(idx, breaks=K, labels=FALSE)
nofNeurons <- 5:10                                # experiment with number of Neurons
scoreCor <- matrix(0, nrow=length(nofNeurons), ncol=K)

system.time(
  for (j in 1:length(nofNeurons)){
    for (i in 1:K){
      cat("processing neurons=",nofNeurons[j],"for fold #", i, "\n")
      valIdx <- which(folds == i, arr.ind=TRUE)
      test <- nBoston[valIdx, ]
      train <- nBoston[-valIdx, ]
      set.seed(12345) # to guarantee repeatable results
      medvNN <- neuralnet(medv ~ ., data = train,  linear.output = TRUE,
                         stepmax=1000000, hidden = nofNeurons[j], act.fct = softplus)
      result <- predict(medvNN, test[1:13])
      scoreCor[j,i] <- cor(result, test$medv)
    } #end::for_i
  } #end::for_j
) #end::system.time
scoreCor <- cbind(nofNeurons, Average=rowMeans(scoreCor), scoreCor)
scoreCor  


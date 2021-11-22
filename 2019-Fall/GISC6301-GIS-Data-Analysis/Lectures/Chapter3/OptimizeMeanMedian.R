##
## Demonstrate the that the minimization properties of the median and mean
##
rm(list=ls())
x <- c(4.7, 4.9, 5.1, 5.2, 5.3, 5.6, 5.9, 6.1, 6.8, 7.2)     # 10 random numbers

## Visualize distribution
hist(x, probability=TRUE, ylim=c(0,1), main="Distribution of 10 random numbers")
rug(x, col="green", lwd=2)
lines(density(x,  bw=0.2), col="red", lwd=2)

##
## Evaluate mean and median functions in dependence of theta
##
theta <- seq(min(x), max(x), length=100)     # sequence of possible theta values
meanFunc <- numeric(length(theta))           # initialize result vectors
medianFunc <- numeric(length(theta))
## evaluate function values
for (i in 1:length(theta)) meanFunc[i] <- sum((x-theta[i])^2)      # squared differences
for (i in 1:length(theta)) medianFunc[i] <-  sum(abs(x-theta[i]))  # absolute differences

##
## Plot both functions and evaluate their minimum values
##
plot(theta, meanFunc, type="l", lwd=2, col="red",                    # squared differences curve
     ylab="meanFunc (red) and medinaFunc (green)",
     main="Evaluate Optimization Properties of Mean and Median")
rug(x, lwd=2)
abline(v=theta[which.min(meanFunc)], col="red", lty=3, lwd=3)        # mean

## Question: is any value inbetween 5.3 < theta < 5.6 a valid median?
lines(theta, medianFunc, lwd=2, col="green")                         # absolute differences curve
abline(v=theta[which.min(medianFunc)], col="green", lty=3, lwd=3)    # median



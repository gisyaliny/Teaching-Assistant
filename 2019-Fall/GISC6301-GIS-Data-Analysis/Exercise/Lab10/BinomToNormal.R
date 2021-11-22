rm(list=ls(all=TRUE))                     # start clean
## Binomial distribution approaching normal distribution
n <- 1000
pi <- 0.1
E <- pi*n                                 # expectation
sdBinom <- sqrt(pi*(1-pi)*n)              # standard deviation
( skewBinom <- (1-2*pi)/sdBinom )         # skewness

## Binomial probabities
binomDf <- data.frame(Count=0:n, prob=dbinom(0:n, size=n, prob=pi))
binomDf <- binomDf[binomDf$prob>0.0001, ] # drop counts with probs close to zero

## Plot binomial distribution with normal curve overlaid
plot(prob~Count, data=binomDf, type="h", lwd=4, ylab="Prob(Count)",
     main=bquote("Binomial Distribtuion with"~~pi==.(pi)~~"and"~~n==.(n)))
lines(spline(binomDf$Count, dnorm(binomDf$Count,mean=E,sd=sdBinom)), col="red")


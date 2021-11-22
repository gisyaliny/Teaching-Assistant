##
## Binomial Distribution
##

n <- 12                                     # define the number of trials
prob <- 0.5                                 # probability of success

(x <- qbinom(0.5, size=n, prob=prob))       # Quantile x_0.5: median

dprobs <- dbinom(0:n, size=n, prob=prob)    # probability successes
barplot(dprobs, names.arg=0:n, main="Binomial Density", xlab="Number of Successes")

cbind(0:n ,dprobs)                          # probability table

pprobs <- pbinom(0:n, size=n, prob=prob)    # cumulative probability distibution. Check option lower.tail
barplot(pprobs, names.arg=0:n, ylim=c(0,1), 
        main="Binomial Cumulative Distribution", xlab="Number of Successes")

pbinom(8, n, prob) - pbinom(3, n, prob)     # probability that x in interval [4,8]
sum(dbinom(4:8, n, prob))                   # dito

(E <- sum(0:n * dprobs))                    # Expectation: n*prob
(V <- sum((0:n-E)^2 * dprobs))              # Variance: n*prob*(1-prob)
(S <- sum(((0:n-E)/sqrt(V))^3 * dprobs))    # Skewness: (1-2*prob)/sqrt(V)

##
## Poisson Distribution
##
xRange <- 0:30
lambda <- 10

(x <- qpois(0.5, lambda=lambda))            # Quantile x_0.5: median

dprobs <- dpois(xRange, lambda=lambda)      # probability of events
barplot(dprobs, names.arg=xRange, main="Poisson Density", xlab="Number of Events")

pprobs <- ppois(xRange, lambda=lambda)      # cumulative probability distribution
barplot(pprobs, names.arg=xRange, ylim=c(0,1),
        main="Poisson Cumulative Distribution", xlab="Number of Events")
cbind(0:30,dprobs)


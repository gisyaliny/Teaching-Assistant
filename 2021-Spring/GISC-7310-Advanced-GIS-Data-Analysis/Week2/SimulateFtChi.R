rm(list=ls())                          # Clear environment
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

## Set degrees of freedom for numerator (df1) and denominator (df2)
df1 <- 15
df2 <- 20

## Set number of simulated random variables
n <- 1000
##
## Simulate and plot the chi^2-distribution
##
x <- rep(NA, n)            # Initialize vector of random variables
for (i in 1:n){
  x[i] <- sum(rnorm(df1)^2)
}
ks.test(x,"pchisq", df1, alternative="two.sided")
plot(ecdf(x)); lines(sort(x), pchisq(sort(x), df1), col="red")

hist(x, breaks= n/20, freq=FALSE, 
     main=bquote(paste("Distribution of ", chi^2," with ", df%==% .(df1))))
lines(sort(x), dchisq(sort(x), df1), col="red")
abline(v=mean(x), lty=5)

##
## Simulate and plot the t-distribution
##
x <- rep(NA, n)            # Initialize vector of random variables
for (i in 1:n){
  x[i] <- rnorm(1)/sqrt(sum(rnorm(df1)^2)/df1)
}
ks.test(x,"pt", df1, alternative="two.sided")
plot(ecdf(x)); lines(sort(x), pt(sort(x), df1), col="red")

hist(x, breaks= n/20, freq=FALSE, 
     main=bquote(paste("Distribution of t with ", df%==% .(df1))))
lines(sort(x), dt(sort(x), df1), col="red")
abline(v=mean(x), lty=5)

##
## Simulate and plot the F-distribution
##
x <- rep(NA, n)            # Initialize vector of random variables
for (i in 1:n){
  x[i] <- (sum(rnorm(df1)^2)/df1)/(sum(rnorm(df2)^2)/df2)
}
ks.test(x,"pf", df1, df2, alternative="two.sided")
plot(ecdf(x)); lines(sort(x), pf(sort(x), df1, df2), col="red")

hist(x, breaks= n/20, freq=FALSE, 
     main=bquote(paste("Distribution of F with ", df[1]%==% .(df1)," and ", df[2]%==% .(df2))))
lines(sort(x), df(sort(x), df1, df2), col="red")
abline(v=mean(x), lty=5)

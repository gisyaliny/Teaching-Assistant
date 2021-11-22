rm(list=ls())
library(ggplot2); library(car)

n <- 5000    # sample size

## Expectaton and variance of the log-normal distribution
mu.y <- 2
var.y <- 0.5

## simmulate log-normal distributed values
df <- data.frame(y= rlnorm(n, meanlog=mu.y, sdlog=sqrt(var.y)) )

bw <- 1
ggplot(df, aes(x=y)) + 
  geom_histogram(binwidth=bw) +
  stat_function(fun=function(x,meanlog,sdlog,n,bw){dlnorm(x,meanlog,sdlog)*n*bw}, 
                    args=list(meanlog=mu.y, sdlog=sqrt(var.y), n=n, bw=bw), n=500, color="red") +
  ylim(0,600)

## log-normality tests using "plnorm"
ks.test(df$y, plnorm, meanlog=mu.y, sdlog=sqrt(var.y))

## normality tests using "pnorm" => rejection
ks.test(df$y, pnorm, mean=mu.y, sd=sqrt(var.y))

## normality tests using no log(y)
ks.test(log(df$y), pnorm, mean=mean(log(df$y)), sd=sd(log(df$y)))

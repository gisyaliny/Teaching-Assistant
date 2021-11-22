################################################################################
##  Evaluate the underlying assumptions of the independent two-sample t-test  ##
################################################################################
rm(list=ls())
# Type of Distribution Pairs
choice <- menu(c("Equal SD: Normal vs Normal","Unequal SD: Normal vs Normal",
                 "Equal SD: t-Distrib vs t-Distrib","Equal SD: Expo vs Expo",
                 "Unequal SD: Normal vs Expo"),
                 graphics = T, title = "Select Population Distribution")

tTestIndiSample <- function(x,y) {
  m <- length(x)
  n <- length(y)
  sp <- sqrt(((m-1)*sd(x)^2+(n-1)*sd(y)^2)/(n+m-2))
  t <- (mean(x)-mean(y))/(sp*sqrt(1/m+1/n))
  return(t)
} # END:tTestIndiSample

x.mean <- 1
y.mean <- 1

## Define input parameter
m <- 10; n <- 10; alpha <- 0.1
n.sample <- 10000
t.val <-numeric(n.sample)

switch(choice,{                                # Equal SD: Normal vs Normal
    pop.x <- rnorm(100000,mean=x.mean,sd=1)
    pop.y <- rnorm(100000,mean=y.mean,sd=1)
  },{                                          # Unequal SD: Normal vs Normal
    pop.x <- rnorm(100000,mean=x.mean,sd=1)
    pop.y <- rnorm(100000,mean=y.mean,sd=100)
  },{                                          # Equal SD: t-Distrib vs t-Distrib
    pop.x <- rt(100000,df=1)
    pop.y <- rt(100000,df=1)
  },{                                          # Equal SD: Expo vs Expo
    pop.x <- rexp(100000,rate=1/x.mean)
    pop.y <- rexp(100000,rate=1/y.mean)
  },{                                          # Unequal SD: Normal vs Expo
    pop.x <- rnorm(100000,mean=x.mean,sd=8)
    pop.y <- rexp(100000,rate=1/y.mean)
} )

n.reject <- 0
for (i in 1:n.sample){
  x <- sample(pop.x,m,replace=F)
  y <- sample(pop.y,n,replace=F)
  t.val[i] <- tTestIndiSample(x,y)
  if (abs(t.val[i]) > qt(1-alpha/2, n+m-2)) n.reject <- n.reject + 1
} #END:for

## Plot simulated density against t-distribution
x.range <- seq(-8,8,by=0.05)
density(t.val)
plot(density(t.val), xlim=c(min(x.range),max(x.range)), ylim=c(0,0.4), lwd=3,
     main=paste("True Significance level", round(n.reject/n.sample,2),
                "against nominal level",round(alpha,2),sep=" "))
lines(x.range,dt(x.range,df=n+m-2),col="red")
abline(v=c(qt(alpha/2, n+m-2),qt(1-alpha/2, n+m-2)),lty=2,col="red")
legend(3.0,0.35,c("simulated distributions","t-distribution (df=18)"),title="Comparison based on:",
       lwd=c(3,1),col=c("black","red"))

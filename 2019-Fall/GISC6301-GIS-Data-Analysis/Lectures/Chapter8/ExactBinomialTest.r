rm(list=ls(all=TRUE))  # start clean

## Exact binomial test
pi.H0 <- 0.2
n <- 100
alpha <- 0.05
## See ?qbinom: The quantile is defined as the smallest value x such 
## that F(x) >= p, where F is the distribution function.
( x.low <- qbinom(alpha/2,size=n,prob=pi.H0) )                  # lower critical value
( x.hi <- qbinom(1-alpha/2,size=n,prob=pi.H0)-1 )               # upper critical value

## Evaluate exact error probability
( pi.low <- pbinom(x.low,size=n,prob=pi.H0) )                   # exact lower prob
( pi.hi <- pbinom(x.hi,size=n,prob=pi.H0,lower.tail=FALSE) )    # exact upper prob

p <- dbinom(0:n,size=n,prob=pi.H0)
plot(0:n,p,type="h",lwd=3,xlab=expression(x[count]),ylab=expression(Pr(X==x)),
     main="Binomial Distribution")
abline(v=pi.H0*n,col="green")
text(pi.H0*n,-0.0015,expression(E[X|H[0]]))
abline(v=c(x.low,x.hi),lty=3,col="red")
text(x.low-1,0.1,bquote(Pr(X<=.(x.low))==.(round(pi.low,5))))
text(x.hi+15,0.1,bquote(Pr(X>=.(x.hi))==.(round(pi.hi,5))))

## confidence intervals arround an observed pi.obs=0.26 estimate
library(Hmisc)
pi.obs <- 0.26                                             # x.obs: pi.obs*n = 26
round(binconf(pi.obs*n, n=n, alpha=alpha, method="all"),3) # => pi.H0 not in interval and reject H0

## Alternatively use function binomial test
binom.test(26, n, p=pi.H0, alternative = "two.sided")      # book scenario
binom.test(26, n, p=pi.H0, alternative = "greater")        # more narrow one-sided perspective

## investigate the beta error for different hypothetical values of pi.H1
#install.packages("binom")
library(binom)                                             # installation may be required

pi.H1 <- seq(0.1, 0.5, by=0.01)                            # value range of pi under H1
beta <- 1 - binom.power(pi.H1, n=n, p=pi.H0, alpha=alpha,
                        alternative = "two.sided", method="exact")

plot(pi.H1, beta, type="l", ylim=c(0,1), yaxt="n",
     xlab=expression(italic(pi[H[1]])), ylab=expression(beta-error),
     main="Beta Error Investigation" )
axis(side=2, at=seq(0,1, by=0.05))
abline(h=1-alpha, col="lavender", lwd=2, lty=5)
abline(v=pi.H0, col="tomato",lwd=2)                        # value pi under H0
abline(v=c(x.low/n,x.hi/n), col="salmon",lty=5)            # critical values of pi
abline(h=0)


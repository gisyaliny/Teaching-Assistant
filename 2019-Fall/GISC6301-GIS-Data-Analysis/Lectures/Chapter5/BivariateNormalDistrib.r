library(mvtnorm)   # Library for Bivariat normal distribution

#########################################################
## Define parameters of bivariate normal distribution  ##
#########################################################
mu.1 <- 1
sigma.1 <- 1
mu.2 <- 2
sigma.2 <- 2
cor.12 <- 0.2

x1.low <- -Inf
x1.high <- 3
x2.low <- -1
x2.high <- 4

## Calculate parameter vectors for bivariate normal distribution
mu <- c(mu.1,mu.2)
x.low <-c(x1.low,x2.low)
x.high <- c(x1.high,x2.high)
covmat <- rbind(c(sigma.1^2, cor.12*sigma.1*sigma.2), c(cor.12*sigma.1*sigma.2, sigma.2^2))

## setup plots
nstd <- 3.5
x1 <- seq(mu.1-nstd*sigma.1,mu.1+nstd*sigma.1,length=201)
x2 <- seq(mu.2-nstd*sigma.2,mu.2+nstd*sigma.2, length = 201)

## Plot univariate distributions
opar <- par(mfrow=c(1,2))
  InBoundPoly <- function(x,mu,sigma,x.low,x.high){
    x <- x[x >= x.low & x <= x.high]
    y <- dnorm(x,mean=mu,sd=sigma)
    x <- c(x.low,x,x.high,x.low)
    y <- c(0,y,0,0)
    polygon(x,y, col="grey90")
  }
  x1.dnorm <- dnorm(x1, mean=mu.1, sd=sigma.1)       # note: the normal density is sometimes denote by smallcaps "phi"
  x1.prob <- pnorm(x1.high, mean=mu.1, sd=sigma.1) - pnorm(x1.low, mean=mu.1, sd=sigma.1)
  plot(x1, x1.dnorm,type="l",xlab=expression(X[1]),
       ylab=bquote(paste(phi(list(x,mu==.(mu.1),sigma==.(sigma.1))))))
  title(main=bquote(paste("Probability: ",.(round(x1.prob,4)))) ,
        sub=bquote(paste(" Within Bounds ", group("[",list(.(x1.low),.(x1.high)),"]"))))
  InBoundPoly(x1,mu.1,sigma.1,x1.low,x1.high)
  abline(v=mu.1,lty=2)
  abline(h=0,lwd=2)
  
  x2.dnorm <- dnorm(x2, mean=mu.2, sd=sigma.2)
  x2.prob <- pnorm(x2.high, mean=mu.2, sd=sigma.2) - pnorm(x2.low, mean=mu.2, sd=sigma.2)
  plot(x2, x2.dnorm,type="l",xlab=expression(X[2]),
       ylab=bquote(paste(phi(list(x,mu==.(mu.2),sigma==.(sigma.2))))))
  title(main=bquote(paste("Probability: ",.(round(x2.prob,4)))) ,
        sub=bquote(paste(" Within Bounds ", group("[",list(.(x2.low),.(x2.high)),"]"))))
  InBoundPoly(x2,mu.2,sigma.2,x2.low,x2.high)
  abline(v=mu.2,lty=2)
  abline(h=0,lwd=2)
par(opar)

if(interactive()) readline(prompt="Hit any key to continue ...")

bi.prob <- pmvnorm(lower=x.low, upper=x.high, mean=mu, sigma=covmat)
dens <- matrix(dmvnorm(expand.grid(x1,x2),mean=mu, sigma=covmat),length(x1),length(x2))
image(x1,x2,dens, col=rev(heat.colors(12, alpha = 0.6)),asp=1,
      xlab=expression(X[1]),ylab=expression(X[2]),main="")
contour(x1,x2,dens,nlevels = 12,add=T)

title(main=bquote(paste("Bivariate Probability: ",.(round(bi.prob,4))," Within Bounds ",
           X[1] == group("[",list(.(x1.low),.(x1.high)),"]"),", and ",X[2] == group("[",list(.(x2.low),.(x2.high)),"]"))),
      sub=bquote(paste("Bivariate Normal Parameters: ",mu == group("[",list(.(mu.1),.(mu.2)),"]"),", ",
          sigma == group("[",list(.(sigma.1),.(sigma.2)),"]"),", and ",rho == .(cor.12))))

abline(v=c(x1.low,x1.high),h=c(x2.low,x2.high), lty=2)


## linear form in x
b0 <- 1   # Play with different parameters to see their effects on the Probs
b1 <- -1

x <- seq((-6-b0)/b1,(6-b0)/b1,length.out=200)  # Adjusted scale to b0 and b1
#x <- seq((-40-b0),(40-b0),length.out=200)  # Adjusted scale to b0 and b1
L <- b0+b1*x

## Different forms for p and (1-p)
## Probs of first category
p1 <- 1/(1+exp(-L))
p2 <- exp(L)/(1+exp(L))

## Probs of second category
np1 <- 1/(1+exp(L))
np2 <- exp(-L)/(1+exp(-L))

## Logistic curve turning point
medX <- -b0/b1

## Plot for different functional forms
layout(matrix(1:2,nrow=1,ncol=2))
  ## Prob(Success)
  plot(x,p1,ylab="1/(1+exp(-L))",ylim=c(0,1),type="l", lwd=2, col="red",main="Pr(X < x)")
  abline(h=c(0,0.5,1),lty=2); abline(v=medX,lty=5)
  ## Prob(Failure)
  plot(x,np1,ylab="1/(1+exp(L))",ylim=c(0,1),type="l", lwd=2,col="blue", main="1 - Pr(X < x)")
  abline(h=c(0,0.5,1),lty=2); abline(v=medX,lty=5)
layout(matrix(1,nrow=1,ncol=1))
## Check equality of logit against L
all.equal(L,log(p1/np1))

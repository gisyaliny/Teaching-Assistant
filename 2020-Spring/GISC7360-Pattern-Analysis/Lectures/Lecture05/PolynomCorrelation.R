rm(list=ls(all=TRUE))  # start clean

##
## Correlation between higher order polynoms
##

## Just positive branch
x1 <- seq(0,2,by=0.05)
x2 <- x1^2
x3 <- x1^3

plot(x2~x1, type="l", col="green", main="Possitive Branch")
lines(x3~x1, col="red")
cor(x2,x1)
cor(x3,x1)

## centered around zero
x1 <- seq(-1,1,by=0.05) # note shift of scale
x2 <- x1^2
x3 <- x1^3

plot(x2~x1, type="l", ylim=range(c(x2,x3)), col="green", main="Centered around Zero")
lines(x3~x1, col="red")
abline(v=0, h=0, lty=5)
cor(x2,x1)
cor(x3,x1)

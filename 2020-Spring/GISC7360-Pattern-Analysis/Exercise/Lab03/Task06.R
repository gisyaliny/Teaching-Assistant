##
## Rejection sampling for an inhomogenous CSR process
## see: Waller p 152
##

## Intensity function
lambda <- function(x,y) return((x+y)/2)    # scale to probability

## define matrix of sample points
ppts <- matrix(NA, nrow=100, ncol=2)
icount <- 1  # counter of accepted sample points

while (icount <= 100){
  ppt <- runif(2,min=0, max=1)                # generate proposal point
  if (runif(1,min=0, max=1) <= lambda(ppt[1],ppt[2])){   # if TRUE => accept point
    ppts[icount,] <- ppt
    icount <- icount+1
  } # end::if
} #end::while
plot(ppts, xlab="x-coordinate", ylab="y-coordinate",
     asp=1, main="Inhomogenous CRS pattern")

## fancy plot
library(spatstat)
inHomoSurf <- as.im(lambda, W=square(1))
pp <- ppp(ppts[,1],ppts[,2], owin=square(1))
plot(inHomoSurf, col=rev(heat.colors(20)), main="Inhomogenous Surface")
plot(pp, add=T)
axis(1); axis(2)


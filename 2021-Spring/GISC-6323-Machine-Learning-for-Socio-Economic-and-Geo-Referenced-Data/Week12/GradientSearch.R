rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

RNGversion("3.5.4"); set.seed(12345)

##
## Define loss and gradient search functions
##
lossFct <- function(y,x,b0,b1){
  sum((y-(b0+b1*x))^2)
} #end::lossFct

gradientUpdateFct <- function(y,x,b0Old,b1Old,alpha){
  # The objective is to move to a minimum loss
  derivb0 <- -2*sum((y - (b0Old+b1Old*x)))    # at optimum equal to zero
  derivb1 <- -2*sum(x*(y - (b0Old+b1Old*x)))  # at optimum equal to zero
  b0New <- b0Old - derivb0*alpha/length(x)
  b1New <- b1Old - derivb1*alpha/length(x)
  return(c(b0New,b1New))
} # end::gradientUpdateFunction

##
## Simulate linear regression data
##
b0 <- 1; b1 <- 1; n <- 200
x <- runif(n,0,2)
y <- b0 + b1*x + rnorm(n, sd=0.25)
summary(lm(y~x))
plot(y~x, ylim=c(0,4), main="Simulated Regression Data")
abline(lm(y~x), col="red")

 ##
## Set parameters controlling the gradient search and history
##
alpha <- 0.05                # Learning rate: update step length. Caution: 0.422 leads to a run-away solution
threshold <- 0.00001         # Change in the loss of subsequent iterations
b0 <- 0; b1 <- 0             # Set starting values
loss <- lossFct(y,x,b0,b1)   # loss at start

##
## Start iterative search
##
abline(a=b0, b=b1, col="green")  # Starting regression line
i <- 1
iterHist <- rbind(NULL,c(i,b0,b1,loss))
lossOld <- Inf

while (abs(lossOld - loss) > threshold){
  i <- i+1; lossOld <- loss
  b <- gradientUpdateFct(y,x,b0,b1,alpha)
  b0 <- b[1]; b1 <- b[2]
  loss <- lossFct(y,x,b0,b1)
  iterHist <- rbind(iterHist,c(i,b0,b1,loss))

  if ((i %% 50)==0){                        # plot results of every 50th iteration
    abline(a=b0, b=b1, col="green")         # updated regression line
    cat(i,b0,b1,loss,"\n")
    Sys.sleep(1)
  } #end::if
} # end::while
abline(lm(y~x), col="red")                  # plot final regression line again

##
## Setup loss surface in parameter space
##
b0Grid <- seq(0,2, by=0.01)
b1Grid <- seq(0,2, by=0.01)
lossValMat <- matrix(NA, nrow=length(b0Grid), ncol=length(b1Grid))
for (i in 1:length(b0Grid))
  for (j in 1:length(b1Grid))
    lossValMat[i,j] <- lossFct(y,x,b0Grid[i],b1Grid[j])
##
## Plot loss surface and iteration history
##
image(b0Grid, b1Grid, lossValMat, ,xlab=expression(b[0]),ylab=expression(b[1]),
      main="Loss Surface and Iteration History")
abline(v=coef(lm(y~x))[1], h=coef(lm(y~x))[2], lty="longdash")
points(iterHist[1:nrow(iterHist),2:3], pch=".", cex=4, col="green")

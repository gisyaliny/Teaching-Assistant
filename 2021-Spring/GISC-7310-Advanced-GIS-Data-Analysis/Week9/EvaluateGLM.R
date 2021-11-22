rm(list=ls(all=TRUE))
###########################################################################################
##  Explore the shape of the logistic probability curve in dependence of the logit=a+b*x ##
###########################################################################################
x <- seq(-9,9,by=0.1)

a <- 1      #Intercept
b <- -0.5   # slope

logit <- a+b*x
prob <- 1/(1+exp(-(logit)))

plot(x,prob,type="l",ylim=c(0,1),col="red",lwd=2,
     ylab="Probability")
abline(v=-a/b,lty=3)            # 0.5 prob point is at -a/b
abline(h=0.5,lty=3)

##  Overlay probit curve
p2lScale <- sqrt(pi^2/3)                 # Scale to match variance of logit and probit functions
prob <- pnorm(logit/p2lScale)            # This is the probit
lines(x,prob,col="blue",lwd=2)
abline(h=c(0,1),lty=2)
legend(2,0.9, legend=c("logit","probit"), lty=1, col=c("red","blue"),bty="n")

###############################################
##   Simulate binary data and estimate model ##
###############################################
rm(list=ls(all=TRUE))
a <- 1; b <- 1                              # Increase slope to see increasing discrimination power
nObs <- 50                                  # Reduce number of observations to see instability of results
#nSize <- rep(1,nObs)                       # Binary distribution
nSize <- rpois(nObs,lambda=20)+1            # Binomial distribution's population at risk
xObs <- runif(nObs,-5,5)                    # Random X
yObs <- vector(mode="numeric", length=nObs) # Initialize vector for Probs

## Generate random 0/1 or rates with given probability and population at risk
yProb <- 1/(1+exp(-(a+b*xObs)))             # Get probabilities by inverse logit in linear predictor
for (i in 1:nObs) {                         # Simulate binomial distributed counts
  yObs[i] <- rbinom(1, nSize[i], yProb[i])
  }
simDF <- data.frame(yObs, yProb, nSize, xObs)

## Fit model binary and binomial model. Binary shortcut: glm(yObs~xObs, ...)
rate <- yObs/nSize
sim.GLM1 <- glm( rate ~ xObs, weights=nSize,     # Rates specification with 
               family=binomial(link="logit"),    # population at risk weights 
               data=simDF)     
summary(sim.GLM1)

## Plot results
plot(xObs, predict(sim.GLM1, type="response"), ylim=c(0,1), type="n")
if (sum(nSize - rep(1,nObs))==0) {             # For binary model
    rug(xObs[yObs==0],side=1)
    rug(xObs[yObs==1],side=3)
  } else {                                     # For binomial model with rates
    points(xObs,yObs/nSize, pch=20) }

## Add predicted line
lines( seq(-5,5,by=0.1),
       predict(sim.GLM1,newdata=data.frame(xObs=seq(-5,5,by=0.1)),type="response"))

##############################
## Poisson model for counts ##
##############################

## Explore shapes of the Poisson distribution
rm(list=ls(all=TRUE))
plot(0:40,dpois(0:40,1), xlab="Count", ylab="Density", ylim=c(0,0.5), type="n",
     main="Shape of Poisson Distributions")
for (i in c(1,2,5,10,20)) lines(spline(0:40,dpois(0:40,i)))
text(1, 0.37, expression(lambda,"  =1"),pos=4)
text(2.4, 0.26, expression(lambda,"  =2"),pos=4)
text(4, 0.20, expression(lambda,"  =5"),pos=4)
text(6.8, 0.14, expression(lambda,"  =10"),pos=4)
text(17, 0.11, expression(lambda,"  =20"),pos=4)

rm(list=ls(all=TRUE))
nObs <- 50
a <- 1; b <- 1
xLow <- 0; xHi <- 3
# Generate Data
xObs <- runif(nObs,xLow,xHi)
lambda <- exp(a+b*xObs)           # inverse link function "exp" for Poisson GLM
yObs <- rep(0,nObs)               # initialize output vector
for (i in 1:nObs) {               # simulate counts subject to expectation lambda
  yObs[i] <- rpois(1,lambda[i])
  }
simDF <- data.frame(yObs,lambda,xObs)

## Estimate Poisson model
sim.GLM <- glm(yObs~xObs, family=poisson(link="log"), data=simDF)
summary(sim.GLM)

## Plot model
plot(xObs, predict(sim.GLM,type="response"), ylim=c(0,max(simDF$yObs)), type="n",
     main="Poisson regression")
points(xObs,yObs,pch=20)
lines( seq(xLow,xHi,by=0.1),
       predict(sim.GLM,newdata=data.frame(xObs=seq(xLow,xHi,by=0.1)),type="response"),
       lwd=2)

##################################################
## Poisson model for counts with overdispersion ##
##################################################

# increase spread of yObs, i.e., overdispersion
addSpread <- runif(nObs, 0, sqrt(lambda))
yObs[yObs < lambda] <- yObs[yObs < lambda] - addSpread[yObs < lambda]
yObs[yObs > lambda] <- yObs[yObs > lambda] + addSpread[yObs > lambda]
yObs[yObs < 0] <- 0
simDF <- data.frame(yObs,lambda,xObs,addSpread)

# Estimate Poisson model with offset
sim.GLM <- glm(yObs~xObs, family=quasipoisson, data=simDF)  # log of offset important
summary(sim.GLM)

# Plot model
points(xObs,yObs,pch=20, col="red")
lines( seq(xLow,xHi,by=0.1),
       predict(sim.GLM,newdata=data.frame(xObs=seq(xLow,xHi,by=0.1)),type="response"),
       col="red")

##########################################
## Poisson model for counts with Offset ##
##########################################
rm(list=ls(all=TRUE))
nObs <- 50
a <- 1; b <- 1
xLow <- 0; xHi <- 3

# Generate Data
xObs <- runif(nObs,xLow,xHi)
shift <- rep(2,nObs)                # Shifts lambda by factor 5
lambda <- exp(a+b*xObs+log(shift))  # the Offset is on the same scale as expected counts
yObs <- rep(0,nObs)
for (i in 1:nObs) {
  yObs[i] <- rpois(1,lambda[i])
  }
simDF <- data.frame(yObs,lambda,xObs,shift)

# Estimate Poisson model with offset
sim.GLM <- glm(yObs~xObs, family=poisson(link="log"), offset=log(shift), data=simDF)  # log of offset important
summary(sim.GLM)

# Plot model
plot(xObs, predict(sim.GLM,type="response"), ylim=c(0,max(simDF$yObs)), type="n",
     main="Poisson model with fixed offset")
points(xObs,yObs,pch=20, col="green")
lines(seq(xLow, xHi, by=0.1), predict(sim.GLM, type="response",
                              newdata=data.frame(xObs=seq(xLow, xHi, by=0.1), shift=2)),
       col="red")


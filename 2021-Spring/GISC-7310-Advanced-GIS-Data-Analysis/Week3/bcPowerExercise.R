##
## Setup script
##
rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
        dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

setwd("E:\\Lectures2021\\GISC7310\\Chapter01")
library(car)
myPower <- foreign::read.spss("DallasTempPower.sav", to.data.frame= TRUE)
myPower <- na.omit(myPower)                           # Remove records with NA's

myPower$powPerDay <- myPower$kWhBill/myPower$DaysBill # calculate kWh per day

##
## Explore the distribution
##
fivenum(myPower$powPerDay)                            # Get limits
hist(myPower$powPerDay)                               # Default binning
hist(myPower$powPerDay, freq=FALSE,                   # density axis
     breaks= seq(15,85, by=5),                        # user binning
     xlab="kW Hours per Day",                         # x-axis label
     main="Distribution Daily Power Consumption")     # graph title
rug(myPower$powPerDay, side=1)                        # add individual obs. on x-axis
lines(density(myPower$powPerDay, bw=5), lwd=2)        # add kernel density smooth distribution

##
## Check normality
##
e1071::skewness(myPower$powPerDay, na.rm=TRUE)
car::qqPlot(myPower$powPerDay)
shapiro.test(myPower$powPerDay)                     # compared to the shapiro test
ks.test(myPower$powPerDay, pnorm,                   # the ks test has not as much power
        mean=mean(myPower$powPerDay), sd=sd(myPower$powPerDay))
##
## Find Box-Cox lambda
##
symbox(~powPerDay, data=myPower,                        # explore different lambda parameters
       powers=c(-2.0,-1.5,-1,-0.5,0.0,0.5,1.0,1.5))
summary(powerTransform(lm(powPerDay~1, data=myPower)))  # test indicates log-transformation sufficient
lambda <- powerTransform(lm(powPerDay~1, data=myPower))$lambda

##
## Box-Cox transform powPerDay
##
myPower$bc.powPerDay <- car::bcPower(myPower$powPerDay, lambda=lambda) # transform with lambda
hist(myPower$bc.powPerDay)
e1071::skewness(myPower$bc.powPerDay)
car::qqPlot(myPower$bc.powPerDay)
shapiro.test(myPower$bc.powPerDay)                   
ks.test(myPower$bc.powPerDay, pnorm, 
        mean=mean(myPower$bc.powPerDay), sd=sd(myPower$bc.powPerDay))

##
## scatterplot incuding loess smoother
##
car::scatterplot(powPerDay~AveTemp, data=myPower)

##
## Simultaneously transform a set of variables 
## by estimating a vectors of lambdas simultaneously
##
summary(lambda <- powerTransform(lm(cbind(powPerDay,AveTemp)~1, data=myPower)))

myPower <- data.frame(myPower,                      # add transformed variables to myPower
                      bcPower(cbind(myPower$powPerDay,myPower$AveTemp), coef(lambda, round=T)))

##
## Handling variables with negative values
##

## Example of the Gamma transformation
zGamma <- function(x, gamma){(x+sqrt(x^2+gamma^2))/2}
x <- seq(-1,4, by=0.1)
gamma <- 2
zx <- zGamma(x, gamma)
plot(x,zx, type="l", xlab="X with negative values", ylab="Gamma Transformed X",
     main="Box-Cox Family with Negative Values and Gamma=2")
abline(v=0, lty=3)
##
## Simulate an positvely skewed distribution with negative values
##
x <- rbeta(100, shape1=1.5, shape2=5)-0.1
hist(x, main="Beta distribution with rbeta(100, shape1=2, shape2=5)-0.1")
fivenum(x)
e1071::skewness(x)
car::qqPlot(x)
shapiro.test(x)                   
ks.test(x, pnorm, mean=mean(x), sd=sd(x))

##
## Use "bcnPower" powerTransform with negative x-values
##
summary(lambda <- powerTransform(lm(x~1), family="bcnPower"))
coef(lambda)

x.bcn <- bcnPower(x, lambda=coef(lambda)[1], gamma=coef(lambda)[2])
hist(x.bcn, main="Box-Cox transformation with Negative Values")

e1071::skewness(x.bcn)
car::qqPlot(x.bcn)
shapiro.test(x.bcn)                   
ks.test(x.bcn, pnorm, mean=mean(x.bcn), sd=sd(x.bcn))


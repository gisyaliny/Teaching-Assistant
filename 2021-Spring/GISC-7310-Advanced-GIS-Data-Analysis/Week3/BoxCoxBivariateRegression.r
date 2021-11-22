rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

library(car)
setwd("E:\\Lectures2021\\GISC7310\\Chapter02")                      # Change working directory

bcReverseMedian <- function(y, lambda){
##  
## Predicted Median of Reverse Box-Cox Transformation
##
  if (abs(lambda) < 1.0E-6)                                         #1.0E-6 matches bcPower=0
    rev.med <- exp(y) else
    rev.med <- (lambda*y+1)^(1/lambda)
  return(rev.med)
} # end:revBoxCoxMed

bcReverseExpectation <- function(y, lambda, sigma){
##
## Predicted Expectation of Reverse Box-Cox Transformation
##
  if (abs(lambda) < 1.0E-6)                                         #1.0E-6 matches bcPower=0 
    rev.exp <- exp(y+0.5*sigma) else
    rev.exp <- (lambda*y+1)^(1/lambda)*(1+0.5*sigma*(1-lambda)/(lambda*y+1)^2)
  return(rev.exp)
} # end:bcRevExp
 
plotBoxCox <- function(y, x, ylambda=1, xlambda=1){ 
###################################################################
## calibrates the model lm(bcPower(y,ylambda)~bcPower(x,xlambda))##
## Performs a prediction in the transformed system               ##
## Maps predicted values back into the untransformed system      ##
###################################################################
  require(car)
  ## Transform both variables
  y.bc <- bcPower(y,ylambda)
  x.bc <- bcPower(x,xlambda)
  ## Calibrate transformed model and perform prediciton
  lm.mod <- lm(y.bc~x.bc)
  sigma <- sum(lm.mod$residuals^2)/(length(lm.mod$residuals)-2)        # estimate residual variance
  x.line <- data.frame(x.bc=seq(min(x.bc),max(x.bc),length.out=1000))  # get smooth line
  pred.line <- predict(lm.mod,x.line)                                  # predict y along line

  y.med.line <- bcReverseMedian(pred.line,ylambda)                     # predicted median
  y.exp.line <- bcReverseExpectation(pred.line,ylambda,sigma)          # predicted expectation
  x.med.line <- bcReverseMedian(x.line$x.bc,xlambda)                   # rescaled independent variable

  ## Plot data in the original measurement system
  plot(x,y,pch=20,                                                 
       xlab=deparse(substitute(x)), ylab=deparse(substitute(y)),   
       main="Predictions after Reverse Box-Cox Transformation")
  ## Plot prediciton lines
  lines(x.med.line,y.med.line,col="green",lwd=2)                       # conditional median
  lines(x.med.line,y.exp.line,col="red",lwd=2)                         # conditional expectation
  legend("topright",title="Conditional Predictions",inset=0.01,bg="white",
         legend=c("Median Method","Expectation Method"),col=c("green","red"),lwd=2)
  pred.pts <- predict(lm.mod)
  pred <- data.frame(y= y, y.pred.med= bcReverseMedian(pred.pts,ylambda), 
                     y.pred.exp= bcReverseExpectation(pred.pts,ylambda,sigma),x=x)
  invisible(pred)
} #end:plotBoxCox

##
## Use of boxcox function from the MASS library to find "best" lambda
## Show profile likelihood
##
Concord <- foreign::read.spss("Concord1.sav", to.data.frame=TRUE)

## First find transformaton for X as input to model for Y
x.bc <- powerTransform(lm(income~1,data=Concord))
( x.lambda <- x.bc$lambda )                                    # (...) echos results on screen
summary(x.bc) 

## Find transformation for Y once X is transformed
y.bc <- powerTransform(lm(water81~bcPower(income,x.lambda), data=Concord))      
( y.lambda <- y.bc$lambda )
summary(y.bc)

##
## Explore relationships by regression and visually
##

## Regression and plot in original units
summary(lm(water81~income, data=Concord))
scatterplot(Concord$income,Concord$water81,
            main="Linear and Lowess Regression based on Orginal Units",
            smooth=list(span = 0.5,lty.smooth=1, col.smooth="red", col.var="red"),
            regLine=list(col="green"))

## Regression and plot in transformed units
summary(lm(bcPower(water81, y.lambda)~bcPower(income, x.lambda), data=Concord))
scatterplot(bcPower(Concord$income,x.lambda),bcPower(Concord$water81,y.lambda),
            main="Linear and Lowess Regression based on Transformed Units",
            smooth=list(span = 0.5,lty.smooth=1, col.smooth="red", col.var="red"),
            regLine=list(col="green"))

## Box-Cox transformed calibration with reverse transformation and plot
plotBoxCox(Concord$water81, Concord$income, y.lambda, x.lambda)


rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

library(car)
setwd("E:\\Lectures2021\\GISC7310\\Chapter02")                 # Change working directory

Concord <- foreign::read.spss("Concord1.sav",to.data.frame=TRUE)

## The "lm" function
help(lm)
reg01 <- lm(water81~income, data=Concord)
class(reg01)
help(summary.lm)
summary(reg01)

## Histogram of model residuals "resid(reg01)" including kernel smoother "density"
hist(resid(reg01), breaks=seq(-2800,7200,by=400), freq=FALSE, 
                  xlab="Model Residuals", main="Shape of the Residual Distribution")
rug(jitter(resid(reg01), factor=1), side=1)
lines(density(resid(reg01), bw=200), lwd=2)

## Residual sum is zero
round(sum(resid(reg01)),14)

## Vector of regression coefficients
coef(reg01)

## Confidence interval around regression coefficients
help(confint)
cbind("Coef"=coef(reg01), confint(reg01, level=0.95))

## Confidence interval around regression line and predicted values
help(predict.lm)
predDf <- data.frame(income=min(Concord$income):max(Concord$income))   # data-frame for independent vars
predDf <- data.frame(predDf,
                     predict(reg01, newdata=predDf, 
                     interval="confidence", level=0.95))               # Line confidence interval & fit
# predDf <- data.frame(predDf,
#                      predict(reg01, newdata=predDf, 
#                              interval="prediction", level=0.95))       # Point confidence interval & fit
#                                           
head(predDf)

## Plot confidence interval
plot(water81~income,data=Concord)
lines(predDf$income,predDf$fit,col="red")                               # predicted value
lines(predDf$income,predDf$lwr,col="green")                             # lower confidence interval limits
lines(predDf$income,predDf$upr,col="green")                             # upper confidence interval limits
abline(h=mean(Concord$water81),v=mean(Concord$income),lty=3)            # Regression line goes thru the means

## Preferred scatterplot with loess curve and marginal box-plots
car::scatterplot(water81~income, data=Concord, 
                 main="Concord Households: Water Consumption against Income", 
                 smooth=list(span = 0.5,lty.smooth=1, col.smooth="red", col.var="red"),
                 regLine=list(col="green"))

## Residuals against predicted values are uncorrelated
plot(resid(reg01)~fitted(reg01))
abline(lm(resid(reg01)~fitted(reg01)))
round(cor(resid(reg01),fitted(reg01)), 6)

##
## Non-linear Modeling using regression splines
## Original code from: http://faculty.marshall.usc.edu/gareth-james/ISL/Chapter%207%20Lab.txt
##
rm(list=ls())
library(ISLR); library(gam); library(splines)
#attach(Wage)
#oldPar <- par()
##
## Polynomial Regression and Step Functions
##
fit <- lm(wage~poly(age,4),data=Wage)              # Use orthogonal polynoms to avoid 
coef(summary(fit))                                 # multicollinearity.

fit2 <- lm(wage~poly(age,4,raw=T),data=Wage)       # Use plain polynoms
coef(summary(fit2))

## Predict using orthognal polynoms
agelims <- range(Wage$age)
age.grid <- seq(from=agelims[1],to=agelims[2])
preds <- predict(fit, newdata=list(age=age.grid), se=TRUE)
se.bands <- cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)

plot(Wage$age, Wage$wage, xlim=agelims, cex=.5,col="darkgrey", 
     main="Degree-4 Polynomial")
lines(age.grid,preds$fit, lwd=2, col="blue")
matlines(age.grid,se.bands, lwd=1, col="blue", lty=3)

## Explore order of polynoms
fit.1 <- lm(wage~age,data=Wage)
fit.2 <- lm(wage~poly(age,2),data=Wage)
fit.3 <- lm(wage~poly(age,3),data=Wage)
fit.4 <- lm(wage~poly(age,4),data=Wage)
fit.5 <- lm(wage~poly(age,5),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)

##
## Splines
##
fit <- lm(wage~ns(age, knots=c(25,40,60)), data=Wage)            # Standard splines
pred <- predict(fit,newdata=list(age=age.grid),se=T)
plot(Wage$age, Wage$wage, col="gray")
lines(age.grid,pred$fit, lwd=2)
lines(age.grid,pred$fit+2*pred$se,lty="dashed")
lines(age.grid,pred$fit-2*pred$se,lty="dashed")

dim(bs(Wage$age, knots=c(25,40,60)))
dim(bs(Wage$age, df=6))
attr(bs(Wage$age, df=6),"knots")

dim(ns(Wage$age, knots=c(25,40,60)))                             # natural spline                         
fit2 <- lm(wage~ns(age,df=4),data=Wage)                          
pred2 <- predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid, pred2$fit,col="red",lwd=2)                       # add natural spline

##
## Smoothing Splines
##
plot(Wage$age, Wage$wage, xlim=agelims, cex=.5, col="darkgrey",
     main="Smoothing Spline")
fit <- smooth.spline(Wage$age, Wage$wage, df=16)
fit2 <- smooth.spline(Wage$age, Wage$wage, cv=TRUE)     # Determine df (i.e., lambda) with LOOCV
fit2$df
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

##
## Generalized Additive Models
##
gam1 <- lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)

gam.m1 <- gam(wage~s(age,5)+education,data=Wage)
gam.m2 <- gam(wage~year+s(age,5)+education,data=Wage)
gam.m3 <- gam(wage~s(year,3)+s(age,5)+education,data=Wage)
anova(gam.m1, gam.m2, gam.m3, test="F")
summary(gam.m3)
plot(gam.m3, se=TRUE,col="blue")


rm(list=ls())  # start clean
library(car); library(scatterplot3d)
school <- foreign::read.spss("StateSchool.sav", to.data.frame=T)

## Scatterplot matrix
scatterplotMatrix(~SAT+Expend+PctSAT, data=school, spread=F) 
cor(school[ , c("SAT","Expend","PctSAT") ])

## Generate 3d scatterplot
y <- school$SAT
x1 <- school$PctSAT
x2 <- school$Expend
my.SAT <- lm(y~x1+x2)
idx.sign <- 1+(resid(my.SAT)>0)

s3d <- scatterplot3d(x=x1,y=x2,z=y,angle=60,
                     xlab="PctSAT",ylab="Expend",zlab="SAT Score",
                     xlim=c(0,100),ylim=c(0,10),zlim=c(700,1200),pch=16, grid=T,
                     color=c("blue","red")[idx.sign], main="State School Expenditure Example")

s3d$plane3d(coefficients(my.SAT),lty="solid")

org <- s3d$xyz.convert(x1,x2,y)
plane <- s3d$xyz(x1,x2,fitted(my.SAT))
segments(org$x,org$y,plane$x,plane$y,col=c("blue","red")[idx.sign])


## Regression SAT on Expend
lm.01 <- lm(SAT~Expend, data=school)
summary(lm.01)

## Regression SAT on Expend controlling for PctSAT
lm.02 <- lm(SAT~Expend+PctSAT, data=school)
summary(lm.02)

## Residual analysis
school$resid.02 <- residuals(lm.02)              # add residuals to data-frame   
school$pred.02 <- predict(lm.02)                 # add predicted values to data-frame

scatterplot(resid.02~Expend, data=school, ylab="Residuals: lm(SAT~Expend+PctSAT)")
scatterplot(resid.02~PctSAT, data=school, ylab="Residuals: lm(SAT~Expend+PctSAT)")
scatterplot(resid.02~pred.02, data=school, ylab="Residuals: lm(SAT~Expend+PctSAT)", 
            xlab="Prediction: lm(SAT~Expend+PctSAT)")

## Refine model to capture quadratic pattern w.r.t. PctSAT
lm.03 <- lm(SAT~Expend+PctSAT+I(PctSAT^2), data=school)  # Note syntax I(PctSAT^2) because ^2 has a special meaning in formulas
summary(lm.03)
school$resid.03 <- residuals(lm.03)  
scatterplot(resid.03~PctSAT, data=school, spread=F, 
            ylab="Residuals: lm(SAT~Expend+PctSAT+I(PctSAT^2))")

## Visualize the non-linear effect of PctSAT
library(effects)
lm.03.eff <- allEffects(lm.03, xlevels=list(PctSAT=min(school$PctSAT):max(school$PctSAT)))
plot(lm.03.eff, "PctSAT", main="Non-linear effect of PctSAT")


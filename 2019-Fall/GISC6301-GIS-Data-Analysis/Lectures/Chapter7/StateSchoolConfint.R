rm(list=ls())  # start clean
library(car)
school <- foreign::read.spss("F:\\Lectures2018\\GISC6301\\Chapter04\\StateSchool.sav", to.data.frame=T)

## Regression SAT on Expend controlling for PctSAT
lm.01 <- lm(SAT~Expend+PctSAT+I(PctSAT^2), data=school)
summary(lm.01)
cbind(coef=coef(lm.01),confint(lm.01, level=0.95))
cbind(coef=coef(lm.01),confint(lm.01, level=0.99))

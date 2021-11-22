library(car); library(effects) # Good tools for regression analysis
rm(list=ls())
##
##  Anscombe4Reg data sets
##  Learning objective: Always inspect the data before running a regression
##
data("anscombe")
## Data 1: Sort of okay
Ans.1 <- lm(y1 ~ x1, data=anscombe)
summary(Ans.1)
scatterplot(y1~x1, reg.line=lm, smooth=FALSE, spread=FALSE, id.n = 0,
            boxplots='xy', cex=2, pch=16, span=0.5, data=anscombe)
residualPlots(Ans.1, fitted=F)

## Data 2: Quadratic relationship
Ans.2 <- lm(y2 ~ x2, data=anscombe)
summary(Ans.2)
scatterplot(y2~x2, reg.line=lm, smooth=FALSE, spread=FALSE,
            id.n = 0, cex=2, pch=16, boxplots='xy', span=0.5, data=anscombe)
residualPlots(Ans.2, fitted=F)

## Data 3: Impact of outlier
Ans.3 <- lm(y3 ~ x3, data=anscombe)
summary(Ans.3)
scatterplot(y3~x3, reg.line=lm, smooth=FALSE, spread=FALSE,
            id.n = 0, cex=2, pch=16, boxplots='xy', span=0.5, data=anscombe)
residualPlots(Ans.3, fitted=F)

## Data 4: Induced relationship by outlier
Ans.4 <- lm(y4 ~ x4, data=anscombe)
summary(Ans.4)
scatterplot(y4~x4, reg.line=lm, smooth=FALSE, spread=FALSE,
            id.n = 0, cex=2, pch=16, boxplots='xy', span=0.5, data=anscombe)
residualPlots(Ans.4, fitted=F)

##
## Campus Crime Analysis using absolute numbers
## Primarily modeling the size effect
##
load("E:\\Lectures2019\\GISC6301\\Chapter12\\RegressionExercise.RData")
row.names(CampusCrime) <- 1:nrow(CampusCrime)
scatterplot(police~crime | priv, reg.line=lm, smooth=FALSE, spread=FALSE,
            id=list(method="mahal", n=2, cex=1, col=carPalette()[-1], location="lr"),
            boxplots='xy', span=0.5, by.groups=TRUE, data=CampusCrime)

# Plot on the log-log scale
scatterplot(police~crime | priv, log="xy", reg.line=lm, smooth=FALSE, spread=FALSE,
            boxplots='xy', span=0.5, by.groups=TRUE, data=CampusCrime)

# Interactive identification and selection of outliers
plot(police~crime, data=CampusCrime, log="xy")
identify(CampusCrime$crime, CampusCrime$police)

# Remove unusual case from the dataset
CampusCrimeClean <- CampusCrime[-c(2,3,20,86), ]
# resequenct row numbers
row.names(CampusCrimeClean) <- 1:nrow(CampusCrimeClean)

# untransformed model
crime1.lm <- lm(police~crime, data=CampusCrimeClean)
summary(crime1.lm)

# Advanced approach with log-log transformed data:
# police <- exp(b0)*crime^b1 ==> log(police)=b0+b1*log(crime)
scatterplotMatrix(~police+crime, data=CampusCrimeClean)
summary(car::powerTransform(lm(cbind(police,crime)~1,data=CampusCrimeClean)))
crime2.lm <- lm(log(police)~log(crime),data=CampusCrimeClean)
summary(crime2.lm)
plot(effects::allEffects(crime2.lm))

# Test slope of b1=1
linearHypothesis(crime2.lm, "log(crime) = 1")
(t.stat <- (0.46573-1)/0.04326)
t.stat^2                            # identical to F statistics
(1-pt(abs(t.stat), df=91)) * 2

##
## Controlling for campus size
##
crime3.lm <- lm(police~crime+enroll, data=CampusCrimeClean)
summary(crime3.lm)

##
## Campus Crime Analysis using relative numbers (rates)
## This analysis is free of size effects
##
CampusCrimeClean$crimeRate <- CampusCrimeClean$crime / CampusCrimeClean$enroll
CampusCrimeClean$polDense <- CampusCrimeClean$police / CampusCrimeClean$enroll

scatterplot(polDense~crimeRate | priv, reg.line=lm, smooth=FALSE, spread=FALSE,
            id = TRUE, boxplots='xy', span=0.5, by.groups=TRUE, data=CampusCrimeClean)

crime4.lm <- lm(polDense~crimeRate, data=CampusCrimeClean)
summary(crime4.lm)
residualPlots(crime4.lm)
influenceIndexPlot(crime4.lm)

# remove rogue observation 76
CampusCrimeClean <- CampusCrimeClean[-76,]
scatterplot(polDense~crimeRate | priv, reg.line=lm, smooth=FALSE, spread=FALSE,
            id = TRUE, boxplots='xy', span=0.5, by.groups=TRUE, data=CampusCrimeClean)

crime5.lm <- lm(polDense~crimeRate, data=CampusCrimeClean)
summary(crime5.lm)
residualPlots(crime5.lm)
influenceIndexPlot(crime5.lm)

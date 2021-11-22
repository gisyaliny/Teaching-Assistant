##
## Everitt Anorexia Data
##
rm(list=ls())
load("E:\\Lectures2019\\GISC6301\\Chapter09\\EverittAnorexia.RData")

## Explore both data sets. Look at both data-frames
head(DepSample)

head(IndepSample)
tail(IndepSample)

##
## Two dependent samples t-test
##
boxplot(DepSample$GAIN, ylab="Weight Gain in Treatment Group")
abline(h=0, lty=3)

## Explore Before/After intervention measurements for DepSample
plot(DepSample$AFTER~DepSample$BEFORE, xlim=c(70,100), ylim=c(70,100), 
     xlab="Weight Before Intervention", ylab="Weight After Intervention")
abline(a=0, b=1, lty=5)             # on change line
abline(v=82, lty=3)                 # low start weight of 82#
title(main="Effect of Intervention on 17 Girls")

## Perform t-test whether intervention lead to a change in weight
## H0: After = Before against H1: After <> Before
t.test(DepSample$AFTER, DepSample$BEFORE, 
       alternative='two.sided', conf.level=.95, paired=TRUE)

## Perform t-test whether intervention lead to an increase in weight
## H0: After <= Before against H1: After > Before
t.test(DepSample$AFTER, DepSample$BEFORE, 
       alternative='greater', conf.level=.95, paired=TRUE)

## Two-sided test as one sample test
t.test(DepSample$GAIN, alternative="greater")


##
## Two independent samples t-test: weight gain in cases against weight gain in untreated controls
##
boxplot(GAIN~Treatment, data=IndepSample, ylab="Weight Gain")
abline(h=0, lty=3)

## Check for variance equality in case and control group
var.test(GAIN ~ Treatment, alternative='two.sided', conf.level=.95, data=IndepSample)

## Alternative Bartlett equality of variance test simmultaneously for 2 or more groups
bartlett.test(GAIN ~ Treatment, data=IndepSample)

## Test for difference in means between treatment and control group
t.test(GAIN~Treatment, alternative='greater', 
       conf.level=.95, var.equal=TRUE, data=IndepSample)

## Check the coding of the Treatment variable
data.frame(IndepSample, as.numeric(IndepSample$Treatment)-1)

## Run independent test within regression framework
anorexia.lm <- lm(GAIN~Treatment, data=IndepSample)
## treatment coefficient: Intercept+TreatmentControl (7.265-7.715=-0.450)
summary(anorexia.lm)






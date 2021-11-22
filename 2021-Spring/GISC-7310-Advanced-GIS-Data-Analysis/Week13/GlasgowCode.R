rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

gw <- foreign::read.dbf("Glasgow.dbf")
head(gw)
gw$RATE2010 <- gw$OBSRES2010/gw$POP2001

##
## Logistic regression with rates
##
glm.01 <- glm(RATE2010~INCOMEDEP+TYPE , weight=POP2001, data=gw, family=binomial)
summary(glm.01)

## Account for over-dispersion
glm.02 <- update(glm.01, family=quasibinomial)
summary(glm.02)

##
## Poisson regression with offset
##
glm.03 <- glm(OBSRES2010~INCOMEDEP+TYPE, data=gw, offset=log(EXPRES2010), family=poisson(link="log"))
summary(glm.03)

## Account for over-dispersion
glm.04 <- glm(OBSRES2009~INCOMEDEP+TYPE, data=gw, offset=log(EXPRES2009), family=quasipoisson(link="log"))
summary(glm.04)


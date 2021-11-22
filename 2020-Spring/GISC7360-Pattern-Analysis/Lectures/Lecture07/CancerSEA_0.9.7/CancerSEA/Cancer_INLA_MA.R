#############################################################################
## Algorithm: INLA Spatial Moving Average Model
## Author: Xiaojun pu
## Note: 1965-1970 SEA-to-SEA migration matrix
##       1970-1994 white male lung cancer data
## Date:7/9/2017
#############################################################################

rm(list=ls(all=TRUE))

library(INLABMA)
require(parallel)                                                       ##mclapply is included in parallel package
require(INLA)
library(CancerSEA)

#############################################################
## Import data
#############################################################
data(cancer);data(mig)

## the proportions of 1979 white male over 50-year old
## (1) https://www.cdc.gov/nchs/data/statab/pop6097.pdf
## (2) https://www.census.gov/newsroom/cspan/1940census/CSPAN_1940slides.pdf
White <- 0.83                                                           ## (2) 83% Whites in 1980
Male <- 109132/224567                                                   ## (1) 1979 Pr(Males)
Over50 <- (5034+4921+4140+3495+2502+1641+918+617)/94482                 ## (1) 1979 Pr(Over50|WhiteMale)
(p.risk <- White*Male*Over50)                                           ## Pr(WhiteMaleOver50)

POP <- cancer$POP1980*p.risk                                         ## Population at risk
RawRate <- cancer$L_WM_P2_CN/POP                                 ## Crude lung cancer rates

# logit offset
adjusted.prob <- cancer$L_WM_P2_EX/POP
logit.adjusted.prob <- log(adjusted.prob/(1-adjusted.prob))  #offset


# Z-tranformed variables
cancer$POPDEN <- cancer$POP1980/cancer$AREA
cancer$logPOPDEN <- log(cancer$POPDEN)
cancer$ZPOPDEN <- (cancer$logPOPDEN-mean(cancer$logPOPDEN))/sd(cancer$logPOPDEN)
cancer$ZRAD_MD <- (cancer$RAD_MD-mean(cancer$RAD_MD))/sd(cancer$RAD_MD)
cancer$ZTOBACCO <- (cancer$TOBACCO-mean(cancer$TOBACCO))/sd(cancer$TOBACCO)


## Adjacency matrix
B <- spdep::poly2nb(shpSEA, queen=F)                   # extract first order neighbors links
Adjacency <- spdep::nb2mat(B, style="S")


############################
## GLM (Binomial regression)
############################
glm1 <- glm(RawRate ~ ZRAD_MD + I(ZRAD_MD^2) + ZPOPDEN + ZTOBACCO + offset(logit.adjusted.prob), family = "binomial", data=cancer, weights=POP)
summary(glm1)

## effect plot
eff.GLM.average <- effects::effect("ZRAD_MD",glm1)
plot(eff.GLM.average)

##################################
## INLA Binomial Regression
##################################
## Define INLA formula
formula.inla <- L_WM_P2_CN ~ 1 + ZRAD_MD + I(ZRAD_MD^2)+ ZPOPDEN + ZTOBACCO + offset(logit.adjusted.prob)
## Run INLA binomial regression model
system.time(model.logistic <- inla(formula.inla, family="binomial", Ntrials=POP, data=cancer,
                                   control.predictor=list(compute=TRUE),
                                   control.compute=list(dic=TRUE, cpo=TRUE)))
round(model.logistic$summary.fixed[,1:5],3)                       ## INLA regression coefficients
model.logistic$dic$dic


##########################################
## INLA Spatial moving average model
##########################################
## Migration Matrix
MMig <- mig[ ,-c(1:8)]                                    ## delete ID and other information
MMig <- t(as.matrix(MMig))                                ## transpose migration matrix (destinations as rows)


## standardize the migration matrix by C-coding scheme
G <- MMig                                                   ## set G as asymmetric migration matrix
n <- nrow(G)
D <- sum(G)
C <- n/D*G
#C <- t(MMig/rowSums(MMig))
#C <- Adjacency                                                  ## S-coding schemed adjacncy matrix

## Index for spatial random effects
idx<-1:508

## Define grid on rho
rrho<-seq(-1, 1, length.out=40)

## Set precision with diffused prior
zero.variance = list(prec=list(initial = 1/100, fixed=TRUE))


## define sma.inla function
sma.inla <- function (yformula, Eformula=NULL, Bformula=NULL, d, W, rho, idx, ...)
{
  y <- as.data.frame(model.matrix(yformula, d))[-1]
  ynam <- names(y)

  # define precision matrix
  IrhoW <- as.matrix(diag(nrow(W)) + rho * W)
  SIrhoW <- solve(diag(nrow(W)) + rho * W)
  IrhoW2 <- t(SIrhoW) %*% SIrhoW

  if (!is.null(Bformula)) {
    Ematrix <- as.data.frame(model.matrix(Eformula, d))[-1]}
  Enam <- names(Ematrix)
  mm <- as.data.frame(IrhoW %*% as.matrix(Ematrix))     # (I+rho*W)*(x_E*beta_E)
  names(mm) <- Enam

  if (!is.null(Bformula)) {
    Bmatrix <- as.data.frame(model.matrix(Bformula, d))[-1]}
  Bnam <- names(Bmatrix)

  d2 <- cbind(y, mm, Bmatrix)

  formula <- paste(ynam, "~",  paste(Enam, collapse = "+"),"+",paste(Bnam, collapse = "+"))
  formula <- as.formula(formula)

  # add spatial moving average model
  formula <- update(formula, . ~ . + f(idx, model = "generic0", Cmatrix = IrhoW2))
  res <- inla(formula, data = d2, ...)
  res$logdet <- as.numeric(determinant(IrhoW2)$modulus)
  res$mlik <- res$mlik + res$logdet/2
  return(res)
}


## example of sma.inla()
system.time(smainla<-mclapply(rrho, function(rho){
  sma.inla(yformula= ~L_WM_P2_CN,
           Eformula= ~ZRAD_MD+I(ZRAD_MD^2)+ZPOPDEN,
           Bformula= ~ZTOBACCO,
           offset= logit.adjusted.prob,
           d=cancer, W=C, rho=rho, idx=idx,
           family = "binomial", Ntrials=POP,
           control.family = list(hyper = zero.variance),
           control.predictor=list(compute=TRUE),
           control.compute=list(dic=TRUE, cpo=TRUE),
           control.inla=list(print.joint.hyper=TRUE),
           #tolerance=1e-20, h=1e-6),
           verbose=FALSE)}))
## Perform complete Bayesian Model Averaging on spatial moving average models
system.time(smabma<-INLABMA(smainla, rrho, 0, impacts = F))

round(smabma$summary.fixed[,1:5], 3)                                   ## INLA SMA regression Coefficients
smabma$rho                                                             ## display the distribution of rho

# check DIC
smabma$dic$dic

# plot estimated parameters
oldpar <- par(mfrow=c(2,3))
plot(smabma$marginals.fixed$"(Intercept)", type="l", cex.lab=1.3,
     xlab=expression(beta[0]), ylab=expression(tilde(p)(paste(beta[0], "|", y))))
plot(smabma$marginals.fixed$"ZRAD_MD", type="l", cex.lab=1.3,
     xlab=expression(beta[ZRAD_MD]), ylab=expression(tilde(p)(paste(beta[ZRAD_MD], "|", y))))
plot(smabma$marginals.fixed$"I(ZRAD_MD^2)", type="l", cex.lab=1.3,
     xlab=expression(beta[ZRAD_MD^2]), ylab=expression(tilde(p)(paste(beta[ZRAD_MD^2], "|", y))))
plot(smabma$marginals.fixed$"ZPOPDEN", type="l", cex.lab=1.3,
     xlab=expression(beta[ZPOPDEN]), ylab=expression(tilde(p)(paste(beta[ZPOPDEN], "|", y))))
plot(smabma$marginals.fixed$"ZTOBACCO", type="l", cex.lab=1.3,
     xlab=expression(beta[ZTOBACCO]), ylab=expression(tilde(p)(paste(beta[ZTOBACCO], "|", y))))
plot(smabma$rho$marginal, type="l", cex.lab=1.3, ylim=c(0,15),
     xlab=expression(rho), ylab=expression(tilde(p)(paste(rho, "|", y))))
par(oldpar)


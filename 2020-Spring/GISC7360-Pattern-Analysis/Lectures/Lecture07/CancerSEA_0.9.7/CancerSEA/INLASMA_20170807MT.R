#############################################################################
## Algorithm: INLA Spatial Moving Average Model
## Authors: Xiaojun Pu & Michael Tiefelsdorf
## Note: 1965-1970 SEA-to-SEA migration matrix
##       1970-1994 white male lung cancer data
## Date:8/12/2017
#############################################################################

rm(list=ls(all=TRUE))
library(INLABMA)
require(parallel)                                                #mclapply is included in parallel package
require(INLA)
library(CancerSEA)

#############################################################
## Import data
#############################################################
data(cancer);data(mig)

## the proportions of 1979 white male over 50-year old
## (1) https://www.cdc.gov/nchs/data/statab/pop6097.pdf
## (2) https://www.census.gov/newsroom/cspan/1940census/CSPAN_1940slides.pdf
# White <- 0.83                                                    # (2) 83% Whites in 1980
# Male <- 109132/224567                                            # (1) 1979 Pr(Males)
# Over50 <- (5034+4921+4140+3495+2502+1641+918+617)/94482          # (1) 1979 Pr(Over50|WhiteMale)
# (p.risk <- White*Male*Over50)                                    # Pr(WhiteMaleOver50)
#
# POP <- cancer$POP1980*p.risk                                     # Population at risk
POPRISK <- cancer$POPATRISK1982
RawRate <- cancer$L_WM_P2_CN/POPRISK                               # Crude lung cancer rates

# logit offset
adjusted.prob <- cancer$L_WM_P2_EX/POPRISK
logit.adjusted.prob <- log(adjusted.prob/(1-adjusted.prob))      #offset


# Z-tranformed variables
cancer$POPDEN <- cancer$POP1980/cancer$AREA
cancer$logPOPDEN <- log(cancer$POPDEN)
cancer$ZPOPDEN <- (cancer$logPOPDEN-mean(cancer$logPOPDEN))/sd(cancer$logPOPDEN)
cancer$ZRAD_MD <- (cancer$RAD_MD-mean(cancer$RAD_MD))/sd(cancer$RAD_MD)
cancer$ZTOBACCO <- (cancer$TOBACCO-mean(cancer$TOBACCO))/sd(cancer$TOBACCO)


## Adjacency matrix
B <- spdep::poly2nb(shpSEA, queen=F)                # extract first order neighbors links
Adjacency <- spdep::nb2mat(B, style="S")

## Migration matrix
G <- mig[ ,-c(1:8)]                                 # remove ID and other information
G <- t(as.matrix(G))                                # transpose migration matrix (destinations as rows)
n <- nrow(G)

Gmin <- matrix(0,n,n)
Gmax <- matrix(0,n,n)
for (i in 1:(n-1))
   for (j in (i+1):n){
      Gmin[i,j] <- Gmin[j,i] <- min(G[i,j],G[j,i])
      Gmax[i,j] <- Gmax[j,i] <- max(G[i,j],G[j,i])
   }

## standardized spatial link matrices
C <- n/sum(G)*G                                       # C-coding scheme
#C <- G/rowSums(G)                                    # W-coding scheme
#C <- n/sum(Gmin)*Gmin
#C <- n/sum(Gmax)*Gmax
#C <- Adjacency                                       # S-coding schemed adjacancy matrix

############################
## GLM (Binomial regression)
############################
glm1 <- glm(RawRate ~ ZRAD_MD + I(ZRAD_MD^2) + ZPOPDEN + ZTOBACCO + offset(logit.adjusted.prob),
            family = "binomial", data=cancer, weights=POPRISK)
summary(glm1)

## effect plot
eff.GLM.average <- effects::effect("ZRAD_MD",glm1)
plot(eff.GLM.average, xlab="z(Median Radon)",ylab="Lung Cancer Rate",
     main="Non-linear Threshold Relationship: Cancer ~ f(Radon)")


##################################
## INLA Binomial Regression
##################################
## Define INLA formula
formula.inla <- L_WM_P2_CN ~ 1 + ZRAD_MD + I(ZRAD_MD^2)+ ZPOPDEN + ZTOBACCO + offset(logit.adjusted.prob)
## Run INLA binomial regression model
system.time(model.logistic <- inla(formula.inla, family="binomial", Ntrials=POPRISK,
                                   data=cancer,
                                   control.predictor=list(compute=TRUE),
                                   control.compute=list(dic=TRUE, cpo=TRUE)))
round(model.logistic$summary.fixed[,1:5],3)                       ## INLA regression coefficients
model.logistic$dic$dic


##########################################
## INLA Spatial moving average model
##########################################
## Index for spatial random effects
idx<-1:n

## Define search grid for distribution of rho
C.eval <- eigen((C+t(C))/2, symmetric=TRUE, only.values = TRUE)$values
plot(C.eval, type="h"); abline(h=0,lwd=2)
rrho<-seq(-1/C.eval[1], -1/C.eval[n], length.out=40)

## Set precision with diffused prior
zero.variance = list(prec=list(initial = 1/100, fixed=TRUE))

## define sma.inla function
sma.inla <- function (yformula, Eformula=NULL, Bformula=NULL, d, W, rho, idx, ...)
{
  # define precision matrix
  IrhoW <- diag(nrow(W)) + rho * W
  IrhoW2 <- solve(IrhoW %*% t(IrhoW))

  formula <- yformula
  d2 <- model.frame(yformula, d)

  if (!is.null(Eformula)) {
    evars <- as.character(Eformula)[2]
    formstr <- paste(".~.+", evars, sep="")
    formula <- update(formula,as.formula(formstr))
    Emat <- as.matrix(model.frame(Eformula, d))
    Edf <- as.data.frame(IrhoW %*% Emat)     # (I+rho*W)*(x_E*beta_E)
    d2 <- data.frame(d2,Edf)
    }

  if (!is.null(Bformula)) {
    bvars <- as.character(Bformula)[2]
    formstr <- paste(".~.+", bvars, sep="")
    formula <- update(formula, as.formula(formstr))
    Bdf <- model.frame(Bformula, d)
    d2 <- data.frame(d2,Bdf)
    }

  # add spatial moving average model
  formula <- update(formula, . ~ . + f(idx, model = "generic0", Cmatrix = IrhoW2))
  res <- inla(formula, data = d2, ...)
  res$logdet <- as.numeric(determinant(IrhoW2)$modulus)
  res$mlik <- res$mlik + res$logdet/2
  return(res)
}


## Hybrid migration moving average estimation with sma.inla()
system.time(smainla<-mclapply(rrho, function(rho){
           sma.inla(
              yformula= L_WM_P2_CN~1,
              Eformula= ~ZRAD_MD+I(ZRAD_MD^2)+ZPOPDEN,
              #Bformula= ~ZTOBACCO,
              offset= logit.adjusted.prob,
              d=cancer, W=C, rho=rho, idx=idx,
              family = "binomial", Ntrials=POP,
              control.predictor=list(compute=TRUE),
              control.compute=list(dic=TRUE, cpo=TRUE),
              control.inla=list(print.joint.hyper=TRUE),
              verbose=FALSE)})
           )
## Perform complete Bayesian Model Averaging on spatial moving average models
system.time(smabma<-INLABMA(smainla, rrho, 0, impacts = F))

round(smabma$summary.fixed[,1:5], 3)                                   # INLA SMA regression Coefficients
smabma$rho[2:4]                                                        # display the distribution of rho

# check DIC
smabma$dic$dic

# plot estimated parameters
old.par <- par(mfrow=c(2,3))
  plot(smabma$marginals.fixed$"(Intercept)", type="l", cex.lab=1.3,
       xlab=expression(beta[0]), ylab=expression(tilde(p)(paste(beta[0], "|", y))))
  abline(h=0, lty=5)
  plot(smabma$marginals.fixed$"ZRAD_MD", type="l", cex.lab=1.3,
       xlab=expression(beta[ZRAD_MD]), ylab=expression(tilde(p)(paste(beta[ZRAD_MD], "|", y))))
  abline(h=0, lty=5)
  plot(smabma$marginals.fixed$"I(ZRAD_MD^2)", type="l", cex.lab=1.3,
       xlab=expression(beta[ZRAD_MD^2]), ylab=expression(tilde(p)(paste(beta[ZRAD_MD^2], "|", y))))
  abline(h=0, lty=5)
  plot(smabma$marginals.fixed$"ZPOPDEN", type="l", cex.lab=1.3,
       xlab=expression(beta[ZPOPDEN]), ylab=expression(tilde(p)(paste(beta[ZPOPDEN], "|", y))))
  abline(h=0, lty=5)
  plot(smabma$marginals.fixed$"ZTOBACCO", type="l", cex.lab=1.3,
       xlab=expression(beta[ZTOBACCO]), ylab=expression(tilde(p)(paste(beta[ZTOBACCO], "|", y))))
  abline(h=0, lty=5)
  plot(smabma$rho$marginal, type="l", cex.lab=1.3,
       xlab=expression(rho), ylab=expression(tilde(p)(paste(rho, "|", y))))
  abline(h=0, lty=5)
par(old.par)


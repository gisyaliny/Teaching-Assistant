## ----prep, fig.width=7, fig.height=5, comment="R>"-----------------------

#############################################################################
## Algorithm: INLA Spatial Moving Average Model
## Authors: Xiaojun Pu & Michael Tiefelsdorf
## Note: 1965-1970 SEA-to-SEA migration matrix
##       1970-1994 white male lung cancer data
## Date:8/12/2017
#############################################################################
rm(list=ls(all=TRUE))
library(doParallel); library(foreach)
library(INLA); library(INLABMA)
library(CancerSEA)

## Set the number of available cores for parallel calculations in SNOW
nOfCores <- detectCores(logical=FALSE)

#############################################################
## Import data
#############################################################
data(cancer);data(mig)

POPRISK <- cancer$POPATRISK1982                 # Population at risk
RawRate <- cancer$L_WM_P2_CN/POPRISK            # Crude lung cancer rates

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
B <- spdep::poly2nb(shpSEA, queen=F)            # extract first order neighbors links
Adjacency <- spdep::nb2mat(B, style="S")

## Migration matrix
G <- mig[ ,-c(1:8)]                             # remove ID and other information
G <- t(as.matrix(G))                            # transpose migration matrix (destinations as rows)
n <- nrow(G)

## Calculate minimum and maximum flow matrices
Gmin <- matrix(0,n,n)
Gmax <- matrix(0,n,n)
for (i in 1:(n-1))
   for (j in (i+1):n){
      Gmin[i,j] <- Gmin[j,i] <- min(G[i,j],G[j,i])
      Gmax[i,j] <- Gmax[j,i] <- max(G[i,j],G[j,i])
   }

## different specifications standardized spatial link matrices
C <- n/sum(G)*G                                 # C-coding scheme
#C <- G/rowSums(G)                              # W-coding scheme
#C <- n/sum(Gmin)*Gmin
#C <- n/sum(Gmax)*Gmax
#C <- Adjacency                                 # S-coding schemed adjacancy matrix


## ----aspatial, fig.width=7, fig.height=5, comment="R>"-------------------

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
formula.inla <- L_WM_P2_CN ~ 1 + ZRAD_MD + I(ZRAD_MD^2)+ ZPOPDEN + ZTOBACCO +
                             offset(logit.adjusted.prob)
## Run INLA binomial regression model
system.time(model.logistic <- inla(formula.inla, family="binomial", Ntrials=POPRISK, data=cancer,
                                   control.predictor=list(compute=TRUE),
                                   control.compute=list(dic=TRUE, cpo=TRUE)))
round(model.logistic$summary.fixed[,1:5],3)                       ## INLA regression coefficients
model.logistic$dic$dic

## ----sma.inla, fig.width=7.5, fig.height=6, comment="R>"-----------------
sma.inla <- function (yformula, Eformula=NULL, d, W, rho, idx, ...)
{
  library(INLA)                                # Local instance needed for SNOW
  # define precision matrix
  IrhoW <- diag(nrow(W)) + rho * W
  # Precision matrix (inverse of covariance matrix)
  IrhoW2 <- solve(tcrossprod(IrhoW))           # slightly faster than solve(IrhoW %*% t(IrhoW))
  formula <- yformula
  d2 <- model.frame(yformula, d)

  # add environmental variables to formula and data.frame
  if (!is.null(Eformula)) {
    evars <- as.character(Eformula)[2]
    formstr <- paste(".~.+", evars, sep="")
    formula <- update(formula,as.formula(formstr))
    Emat <- as.matrix(model.frame(Eformula, d))
    Edf <- as.data.frame(IrhoW %*% Emat)     # (I+rho*W)*X_E
    d2 <- data.frame(d2,Edf)
    }

  formula <- update(formula, . ~ . + f(idx, model = "generic0", Cmatrix = IrhoW2))
  # add spatial moving average model
  res <- inla(formula, data = d2, ...)
  res$logdet <- as.numeric(determinant(IrhoW2)$modulus)
  res$mlik <- res$mlik + res$logdet/2
  return(res)
}

## ----spatial, fig.width=7.5, fig.height=6, comment="R>"------------------

##########################################
## INLA Spatial moving average model
##########################################
## Index for spatial random effects
idx<-1:n

## Define search grid for distribution of rho
C.eval <- eigen((C+t(C))/2, symmetric=TRUE, only.values = TRUE)$values
plot(C.eval, type="h", ylab="Eigenvalue", main="Spectrum of Eigenvalues"); abline(h=0,lwd=2)
# rho_min = -1/lambda_max and rho_max=-1/lambda_min
rrho <- seq(-1/C.eval[1], -1/C.eval[n], length.out=40)  

## Set precision with diffused prior
zero.variance = list(prec=list(initial = 1/100, fixed=TRUE))

## Get ready for parallel execution of sma.inla
cl <- makeCluster(nOfCores)    # Enable all cores in doParallel
registerDoParallel(cl)         # May trigger firewall warning in Windows

## Hybrid migration moving average estimation with sma.inla()
system.time(
  smainla <- foreach(rho=rrho) %dopar% {
               sma.inla(yformula= L_WM_P2_CN~ZTOBACCO,
                        Eformula= ~ZRAD_MD+I(ZRAD_MD^2)+ZPOPDEN,
                        offset= logit.adjusted.prob,
                        d=cancer, W=C, rho=rho, idx=idx,
                        family = "binomial", Ntrials=POPRISK,
                        control.predictor=list(compute=TRUE),
                        control.compute=list(dic=TRUE, cpo=TRUE),
                        control.inla=list(print.joint.hyper=TRUE),
                        verbose=FALSE)
             } #end::foreach
 ) #end::system.time
stopCluster(cl)               # Turn clusters off

## Perform complete Bayesian Model Averaging on spatial moving average models
system.time(smabma<-INLABMA(smainla, rrho, 0, impacts = F))

round(smabma$summary.fixed[,1:5], 3)                             # INLA SMA regression Coefficients
smabma$rho[2:4]                                                  # display the distribution of rho

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


rm(list=ls(all=T))
# install.packages("G:\\UTD_Classes\\2020Spring\\GISC7360_Pattern_Analysis\\Lecture07\\CancerSEA_0.9.7.tar.gz", repos=NULL)
library(CancerSEA); library(spdep); library(car)

##
## Merge external data to spatial object
##
data(shpSEA); data(shpStates); data(shpNA); data(cancer)
shpSEA <- sp::merge(shpSEA, cancer, by.x="SEQID", by.y="SEQID")
shpSEA$POPDEN <- shpSEA$POP1980/shpSEA$REALSQMILE
summary(shpSEA)

## Extract connectivity info. 
cnt <- coordinates(shpSEA)                                    # get tile centroids
B <- spdep::poly2nb(shpSEA, queen=F)                          # extract first order neighbors links
summary(B)

## Check for heteroscedasticity
pro.het <- lmHetero(P_WM_P2_RT~log(POPDEN)+B_WM_P1_RT+log(RAD_MD) | log(POP1980/2), data=shpSEA)
summary(pro.het)

## Continue with lm and weighted lm by using pro.het$weights as weights 
pro.wlm <- lm(P_WM_P2_RT~log(POPDEN)+B_WM_P1_RT+log(RAD_MD), data=shpSEA,
              weights = pro.het$weights)
summary(pro.wlm)

##
## Compare weighted and unweighted SAR model
##
proLM.SAR <- spautolm(P_WM_P2_RT~log(POPDEN)+B_WM_P1_RT+log(RAD_MD), 
                      data=shpSEA, family="SAR", 
                      listw=nb2listw(B,style="W"))
summary(proLM.SAR)
proWLM.SAR <- spautolm(P_WM_P2_RT~log(POPDEN)+B_WM_P1_RT+log(RAD_MD), 
                       data=shpSEA, family="SAR", 
                       weights = pro.het$weights,
                       listw=nb2listw(B,style="W"))
summary(proWLM.SAR)

##
## Estimate Gaussian autoregressive spatial models in the C-coding scheme
##

## Simultaneous Autoregressive Model
proWLM.SAR <- spautolm(P_WM_P2_RT~log(POPDEN)+B_WM_P1_RT+log(RAD_MD), 
                       data=shpSEA, family="SAR", 
                       weights = pro.het$weights,
                       listw=nb2listw(B,style="C"))
summary(proWLM.SAR)

## Moving Average Autoregressive Model
proWLM.SMA <- spautolm(P_WM_P2_RT~log(POPDEN)+B_WM_P1_RT+log(RAD_MD), 
                       data=shpSEA, family="SMA", 
                       weights = pro.het$weights,
                       listw=nb2listw(B,style="C"))
summary(proWLM.SMA)

## Conditional Autoregressive Model
proWLM.CAR <- spautolm(P_WM_P2_RT~log(POPDEN)+B_WM_P1_RT+log(RAD_MD), 
                       data=shpSEA, family="CAR", 
                       weights = pro.het$weights,
                       listw=nb2listw(B,style="C"))
summary(proWLM.CAR)

##
## Econmetric Model Specifications. No heteroscedasticity is considered
##

## Lagrange multiplier model diagnositic
summary(lm.LMtests(pro.lm, listw= nb2listw(B,style="W"), 
                   test=c("LMerr","LMlag","SARMA")))

## SAR (Spatial lag model)
pro.LAG <- lagsarlm(P_WM_P2_RT~log(POPDEN)+B_WM_P1_RT+log(RAD_MD), data=shpSEA, 
                    type="lag", listw=nb2listw(B,style="W")) 
summary(pro.LAG)

hist(residuals(pro.LAG))
moran.mc(residuals(pro.LAG), listw=nb2listw(B,style="W"), nsim = 999)
# plot(shpNA, xlim = SEA.box[1,], ylim = SEA.box[2,],col = grey(0.7),bg = "powderblue",axes = T)

mapBiPolar(residuals(pro.LAG), shpSEA, neg.breaks=4, pos.breaks=4, break.value=0,
           map.title="Spatial Lag Residuals of Prostate Cancer Model",
           legend.title="Lag Residuals", add.to.map=F)

## Spatial Durbin lag model SDM (SAR without common factor constraint)
pro.MIX <- lagsarlm(P_WM_P2_RT~log(POPDEN)+B_WM_P1_RT+log(RAD_MD), data=shpSEA, 
                    type="mixed", listw=nb2listw(B,style="W"))
summary(pro.MIX)

## Spatial autoregressive model SEM
pro.ERR <- errorsarlm(P_WM_P2_RT~log(POPDEN)+B_WM_P1_RT+log(RAD_MD), data=shpSEA, 
                      listw=nb2listw(B,style="W"))
summary(pro.ERR)
## Nested comparison pro.LAG and pro.ERR models against pro.MIX
anova(pro.LAG, pro.MIX)
anova(pro.ERR, pro.MIX)


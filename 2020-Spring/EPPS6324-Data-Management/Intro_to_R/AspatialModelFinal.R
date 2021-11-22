install.packages("http://thinkr.spatialfiltering.com/Downloads/NetworkSAC_0.1.3.tar.gz", repos = NULL)
rm(list=ls())
library(NetworkSAC); library(car); library(coefplot)
data("mig2010")
data("ChinaDf")
data("shpChina")
## Province Area
# for (i in 1:31) print(shpChina@polygons[[i]]@Polygons[[1]]@area)
# rgdal::writeOGR(shpChina, dsn=getwd(), layer="ChinaProvinces",
#                 overwrite_layer=T, driver="ESRI Shapefile")
## Province Area in sq km
ChinaDf$Area <- c(16387.23,11688.27,187402.86,156387.84,1144228.25,145610.44,
                  190644.34,453531.13,6889.81,101750.31,102920.38,140410.59,
                  122534.35,167268.77,155471.25,165586.08,186188.80,212421.39,
                  178253.02,237028.84,34164.55,82530.77,484318.47,176287.22,
                  383810.94,1131040.00,205903.03,404959.56,715640.31,51837.23,
                  1627339.63)
ChinaDf$PopDens <- ChinaDf$POP/ChinaDf$Area

## Fix Province Abbreviations 2 x "HB" and 3 x "HN"
data.frame(ChinaDf$Prov,ChinaDf$AbbreProv)
labels <- c("BJ","TJ","HEB","SX","NM","LN","JL","HLJ","SH","JS","ZJ","AH","FJ",
            "JX","SD","HEN","HUB","HUN","GD","GX","HAI","CQ","SC","GZ","YN","XZ",
            "SHX","GS","QH","NX","XJ")
ChinaDf$ProvLab <- factor(ChinaDf$SeqID, labels=labels)
data.frame(ChinaDf$Prov,ChinaDf$ProvLab)
## Province area in sq km

prepIJDf <- function(df, mij=NULL, dij=NULL, logTrans=FALSE, zTrans=FALSE){
  ##############################################################################
  ## Indices:
  ##           '.i'    for origin 
  ##           '.j'    for destination
  ##           '.ij'   for origin/destination ratio
  ##           '.ijF'  factor variable
  ##           '.ijL'  log-transformed variable
  ##           '.ijLZ' log- and z-transformed variable
  ##           '.ijZ'  z-transformed variable
  ## Case treatments:
  ##            1 Factor
  ##            2 NoFactor-possitive
  ##            3 NoFactor-possitive-log
  ##            4 NoFactor-possitive-log-scale
  ##            5 NoFactor-possitive-scale
  ##            6 NoFactor
  ##            7 NoFactor-scale
  ##############################################################################
  if(!is.data.frame(df)) stop("Function prepIJDf: Input not a dataframe")
  
  ## Setup origin and destination ids and unique record id with the format: 
  ## 'ij' for n < 10, iijj' for n < 100 and 'iiijjj' for n < 1000 etc.
  recId <- expand.grid(1:nrow(df),1:nrow(df))
  odDf <- data.frame(ID.i=recId$Var1, ID.j=recId$Var2,
                     ID.ij=recId$Var1*10^trunc(log(nrow(df),base=10)+1)+recId$Var2)

  ## Add vectorized migration flows
  if (is.null(mij)) { odDf <- data.frame(odDf, mij=NA) } else {
    odDf <- data.frame(odDf, mij=matrix(as.matrix(mij, nrow=nrow(mij)^2)) ) }

  ## Add vectorized distances
  if (is.null(dij)) { odDf <- data.frame(odDf, dij=NA) } else {
    odDf <- data.frame(odDf, dij=matrix(as.matrix(dij, nrow=nrow(dij)^2)) ) }

  ## Cycle over all variables
  dfNames <- names(df)  
  for (i in 1:length(dfNames)) {
    odVec <- expand.grid(df[,i],df[,i])

    if (is.factor(df[,i])) {                              # process factors
      odVar <- data.frame(odVec[,1],odVec[,2])
      names(odVar) <- c(paste0(dfNames[i],".iF"), paste0(dfNames[i],".jF"))
      odVar[,1] <- factor(odVec[,1], labels=levels(df[,i]))
      odVar[,2] <- factor(odVec[,2], labels=levels(df[,i]))
    } else { 
      if(!(any(df[,i] <= 0))){                      # process all metric possitive variables                                
        if (!logTrans & !zTrans){
          odVar <- data.frame(odVec[,1],odVec[,2],log( odVec[,1]/odVec[,2]))
          names(odVar) <- c(paste0(dfNames[i],".i"),paste0(dfNames[i],".j"),
                            paste0(dfNames[i],".ijL"))
          } 
        if (logTrans & !zTrans){
          odVar <- data.frame(log(odVec[,1]),log(odVec[,2]),log(odVec[,1]/odVec[,2]))
          names(odVar) <- c(paste0(dfNames[i],".iL"),paste0(dfNames[i],".jL"),
                            paste0(dfNames[i],".ijL"))        
          }
        if (logTrans & zTrans){
          odVar <- data.frame(log(odVec[,1]),log(odVec[,2]),log( odVec[,1]/odVec[,2]))
          odVar <- as.data.frame(scale(odVar))
          names(odVar) <- c(paste0(dfNames[i],".iLZ"),paste0(dfNames[i],".jLZ"),
                            paste0(dfNames[i],".ijLZ"))           
          }
        if (!logTrans & zTrans){
          odVar <- data.frame(odVec[,1],odVec[,2],log( odVec[,1]/odVec[,2]))
          odVar <- as.data.frame(scale(odVar))
          names(odVar) <- c(paste0(dfNames[i],".iZ"),paste0(dfNames[i],".jZ"),
                            paste0(dfNames[i],".ijLZ")) 
          }           
        } else {
        if (!zTrans){
          odVar <- data.frame(odVec[,1],odVec[,2])
          names(odVar) <- c(paste0(dfNames[i],".i"),paste0(dfNames[i],".j"))             
        }
        if (zTrans){
          odVar <- data.frame(odVec[,1],odVec[,2])
          odVar <- as.data.frame(scale(odVar))
          names(odVar) <- c(paste0(dfNames[i],".iZ"),paste0(dfNames[i],".jZ"))}
      }##end::variable support
    } ##end::isFactor
    odDf <- data.frame(odDf, odVar)
  } #end::for
  return(odDf)
} #end::prepIJDf
##
## Evaluate for Multicollinearity associated with log(AgraLandP)
##
vars <- c("POP","PopDens","AgraLandP","Income","UnempP","IllitP","LiveArea","TertEmpP","ExGDP",
          "TempAve","HumidP","Precip","SunDays","TempServ")
China <- log(ChinaDf[,vars])
summary(lm(AgraLandP~., data=China))
vif(lm(AgraLandP~., data=China))
summary(lm(AgraLandP~POP+Income+TempAve+PopDens, data=China))
summary(lm(Income~., data=China))
summary(lm(UnempP~., data=China))

##
## Prepare and reformat the variables for migration analysis
##
dij <- sp::spDists(as.matrix(ChinaDf[,c("LongPop","LatPop")]), longlat= TRUE)  # greate cricle distance based on population centroids
vars <- c("ProvLab","POP","PopDens","AgraLandP","Income","UnempP","IllitP","TertEmpP","ExGDP","LiveArea",
          "TempAve","HumidP","Precip","SunDays","TempServ")

China <- ChinaDf[,vars]
odDf <- prepIJDf(China, mij=t(mig2010[ ,-1]), dij=dij, logTrans=TRUE, zTrans=FALSE)

## drop records of internal flows
odDfNoDiag <- subset(odDf, ID.i!=ID.j)

glm.control(maxit=50, trace=0)

##
## Test models
##
migModel01 <- MASS::glm.nb(mij~bcPower(dij,0.09)+POP.iL+POP.jL+PopDens.iL+PopDens.jL+
                           UnempP.iL+UnempP.jL+Income.iL+
                           Income.jL+IllitP.iL+IllitP.jL+TempServ.iL+
                           AgraLandP.iL+AgraLandP.jL+
                           TempServ.jL+ExGDP.iL+ExGDP.jL, data=odDfNoDiag)
summary(migModel01)
vif(migModel01)
coefplot::coefplot(migModel01, intercept=F)

## Test model with differentials
migModel02 <- MASS::glm.nb(mij~bcPower(dij,0.02)+POP.iL+POP.jL+PopDens.ijL+UnempP.ijL+Income.ijL+
                               IllitP.ijL+AgraLandP.ijL+TempServ.ijL+ExGDP.ijL, data=odDfNoDiag)
summary(migModel02)
vif(migModel02)
coefplot::coefplot(migModel02, intercept=F)

## Test model with differentials and origin/destinaiton constraints
migModel03 <- MASS::glm.nb(mij~ProvLab.iF+ProvLab.jF+ bcPower(dij,0.48)+exp(PopDens.ijL)+
                               exp(Income.ijL)+exp(IllitP.ijL)+exp(ExGDP.ijL), 
                               contrast = list(ProvLab.iF="contr.sum", ProvLab.jF="contr.sum"),
                               data=odDfNoDiag)
summary(migModel03)
vif(migModel03)
coefplot::coefplot(migModel03, intercept=F)

## Poisson doubly constrained model
# migModel03 <- glm(mij~ProvLab.iF+ProvLab.jF+ bcPower(dij,0.48)+exp(PopDens.ijL)+
#                       exp(Income.ijL)+exp(IllitP.ijL)+exp(ExGDP.ijL), 
#                       family=quasipoisson,
#                       contrast = list(ProvLab.iF="contr.sum", ProvLab.jF="contr.sum"),
#                       data=odDfNoDiag)
# summary(migModel03)
# vif(migModel03)

## Check for glm.nb biases
ObsInflow <- tapply(odDfNoDiag$mij, odDfNoDiag$ProvLab.jF, sum, simplify = T)
PredInflow <- tapply(predict(migModel03, type="response"),odDfNoDiag$ProvLab.jF, sum, simplify = T)
cbind(ObsInflow,round(PredInflow,0))

ObsOutflow <- tapply(odDfNoDiag$mij, odDfNoDiag$ProvLab.iF, sum, simplify = T)
PredOutflow <- tapply(predict(migModel03, type="response"),odDfNoDiag$ProvLab.iF, sum, simplify = T)
cbind(ObsOutflow,round(PredOutflow,0))

## Start aspatial model
vars <- c("ProvLab","POP","PopDens","AgraLandP","Income","UnempP","IllitP","TertEmpP","ExGDP","LiveArea",
          "TempAve","HumidP","Precip","SunDays","TempServ")
migModelStart <- MASS::glm.nb(mij~log(dij)+
                                  POP.iL+POP.jL+
                                  PopDens.iL+PopDens.jL+
                                  AgraLandP.iL+AgraLandP.jL+
                                  Income.iL+Income.jL+
                                  UnempP.iL+UnempP.jL+
                                  IllitP.iL+IllitP.jL+
                                  TertEmpP.iL+TertEmpP.jL+
                                  ExGDP.iL+ExGDP.jL+
                                  LiveArea.iL+LiveArea.jL+
                                  TempAve.iL+TempAve.jL+
                                  HumidP.iL+HumidP.jL+
                                  Precip.iL+Precip.jL+
                                  SunDays.iL+SunDays.jL+
                                  TempServ.iL+TempServ.jL,
                             data=odDfNoDiag)
summary(migModelStart)
vif(migModelStart)
coefplot::coefplot(migModelStart, intercept=F)

## Stripped Model with origin characteristics and destination attributes
migModelStrip <- MASS::glm.nb(mij~log(dij)+
                                  POP.iL+POP.jL+
                                  PopDens.iL+PopDens.jL+
                                  Income.iL+Income.jL+
                                  UnempP.iL+UnempP.jL+
                                  IllitP.iL+IllitP.jL+
                                  TertEmpP.iL+TertEmpP.jL+
                                  ExGDP.iL+ExGDP.jL+
                                  TempAve.iL+TempAve.jL+
                                  Precip.iL+Precip.jL+
                                  SunDays.iL+SunDays.jL,
                              data=odDfNoDiag)
summary(migModelStrip)
vif(migModelStrip)
coefplot::coefplot(migModelStrip, intercept=F)

## Stripped Model with origin/destination differentials
migModelDif <- MASS::glm.nb(mij~log(dij)+
                                POP.iL+POP.jL+
                                PopDens.ijL+
                                Income.ijL+
                                UnempP.iL+
                                TertEmpP.ijL+
                                ExGDP.ijL+
                                TempAve.ijL+
                                SunDays.ijL,
                              data=odDfNoDiag)
summary(migModelDif)
vif(migModelDif)
coefplot::coefplot(migModelDif, intercept=F)

##
## Build final model
##
## find optimal lambda by grid search
lambda <- seq(-0.3, 0.3, 0.01)      
LR <- c()                    # Likelihood ratio
for (i in 1:length(lambda)){
  print(lambda[i])
  migModel <- MASS::glm.nb(mij~bcPower(dij,lambda[i])+
                                POP.iL+POP.jL+
                                PopDens.ijL+
                                Income.ijL+
                                UnempP.iL+
                                TertEmpP.ijL+
                                ExGDP.ijL+
                                TempAve.ijL+
                                SunDays.ijL,
                              data=odDfNoDiag)
  LR[i] <- migModel$twologlik
}

lambda[which.max(LR)]                  #find optimal lambda
plot(lambda, LR, type="l", ylab="log-likelihood", xlab=expression(lambda))
abline(v = lambda[which.max(LR)], col="red", lwd=3, lty=2)
text(0.07, -15898, expression(paste("max(", lambda, ")=0.02")),cex = 1.5)

## Conditional distance boxplot before Box-Cox Transformation
iOrdProv <- reorder(odDfNoDiag$ProvLab.i, odDfNoDiag$dij, median)    #sort province by median distance
boxplot(dij~iOrdProv, data=odDfNoDiag,
        main="Interprovincial Distance Distributions by Origin\nbefore Box-Cox Transformation",
        xlab="Province", ylab="Origin-Destination Distance between Population Centroid", las=2)

## boxplot after Box-Cox transformation
boxplot(bcPower(odDfNoDiag$dij,0.02)~iOrdProv, data=odDfNoDiag,
        main="Interprovincial Distance Distributions by Origin\nafter Box-Cox Transformation",
        xlab="Province", ylab="Origin-Destination Distance between Population Centroid", las=2)

migModelFinal <- MASS::glm.nb(mij~bcPower(dij,0.02)+
                              POP.iL+POP.jL+
                              PopDens.ijL+
                              Income.ijL+
                              UnempP.iL+
                              TertEmpP.ijL+
                              ExGDP.ijL+
                              TempAve.ijL+
                              SunDays.ijL,
                            data=odDfNoDiag)
summary(migModelFinal)
vif(migModelFinal)
coefplot::coefplot(migModelFinal, sort="natural", intercept=F)

migModelQuasiPois <- glm(mij~bcPower(dij,0.02)+
                        POP.iL+POP.jL+
                        PopDens.ijL+
                        Income.ijL+
                        UnempP.iL+
                        TertEmpP.ijL+
                        ExGDP.ijL+
                        TempAve.ijL+
                        SunDays.ijL,
                     family=quasipoisson(),
                     data=odDfNoDiag)
summary(migModelQuasiPois)


# ## Origin and destination specifications should be slightly negatively correlated
# ## For numerical stable models VIFs of origin characterisitcs and destination attributes 
# ## should be identical 
# plot(ToIncome~FromIncome, data=migDf2)
# cor(migDf2$ToIncome,migDf2$FromIncome)
# x <- rexp(100, rate=1)
# hist(x)
# xx <- expand.grid(x,x)
# cor(xx)
# xx <- xx[!xx$Var1==xx$Var2, ] # removing diagonal induces slight negative correlation
# cor(xx)


rm(list=ls(all=T))
library(maptools); library(gstat); library(RColorBrewer)
setwd("F:\\Lectures2019\\GISC7360\\Labs\\Lab03\\KanasDEM")              # Directory with data files

zTransCoord <- function(x,xmean,xsd,reverse=F){  
  ###############################################################################
  ## Objective: z-transforms a coordinate if reverse==F and scales the         ##
  ##            coordinate back if reverse=T                                   ##                                                          ##
  ## x      vector of coordinates                                              ##
  ## xmean  centers around xmean                                               ##
  ## xsd    scale factor as standard deviation                                 ##
  ###############################################################################
  if (reverse == F) x <- (x-xmean)/xsd
  else x <- x*xsd+xmean
  return(x)
} #end:zTransCoord

makeTrendPolyForm <- function(baseForm=Z~1, coordForm=~X+Y, shpDf=NULL, polyDeg=1){
  ###############################################################################
  ## Objective: Generate formula for polygonial trend-surface model            ##
  ## Return:    Trendsurface formula object                                    ##  
  ##                                                                           ##
  ## Input:     baseForm  Basic formula defining the dependent variable with   ##
  ##                      an intercept, i.e., Z~1. Or alternatively            ##
  ##                      with covariables, e.g., Z~V1+V2+I(V2^2)              ##
  ##            coordForm Right-hand formula defining the longitude and        ## 
  ##                      latitude coordinate names, i.e., ~X+Y                ##
  ##                      Notes: [a] coordinates need to be properly scaled    ##
  ##                                 to avoid any numerical stability issues   ##
  ##                             [b] short variable names, such as X+Y, are    ##
  ##                                 preferred                                 ##
  ##            shpDf     optional dataframe (perhaps in geo-object) to        ##
  ##                      the variables names in baseForm and coordForm        ##
  ##            polyDeg   Degree of the trendsurface model (minimum is 1)      ##
  ##                                                                           ##
  ## Example:   makeTrendPolyFct(elev~rivDist, ~long+lat, kansas, polyDeg=3)   ##
  ##                                                                           ##
  ###############################################################################
  
  ## Basic input check
  if (missing(baseForm) || class(baseForm) != "formula") stop("'baseForm' missing or incorrect")
  if (missing(coordForm) || class(coordForm) != "formula" || length(all.vars(coordForm)) != 2)
      stop("'coordForm' missing or incorrect")
  if (as.integer(polyDeg) < 1L ) stop("'polyDeg' needs to be integer of 1 or above")
  ## shpDf declare check input variables
  if (!is.null(shpDf)){
    if (class(try(model.frame(baseForm, shpDf), T)) != "data.frame") stop("Incorrect variables in 'baseFrom'")
    if (class(try(model.frame(coordForm, shpDf), T)) != "data.frame") stop("Incorrect variables in 'coordFrom'")
  }
  ## Build string with the trend-surface coordinates
  expo <- expand.grid(0L:polyDeg, 0L:polyDeg)
  expo <- expo[rowSums(expo) <= polyDeg, ]    # make sure only terms up to polyDeg enter the formula
  expo <- expo[-1,]                           # exclude x^0*y^0
  expo <- expo[order(rowSums(expo)), ]        # make sure formula is sorted by the degree of the polynomial
  formStr <- "~ ."
  xy <- all.vars(coordForm)  
  for (i in 1: nrow(expo)) {
    formStr <- paste(formStr, " + I(",xy[1],"^",expo[i,1],"*",xy[2],"^",expo[i,2],")", sep="")  
  }  
  ## Merge covariate and polynomial formulas
  polyForm <- update(baseForm,as.formula(formStr))
  
  return(polyForm)
} #end::makeTrendPolyForm

biPolarColorRamp <- function(varName,pos.breaks=4,neg.breaks=pos.breaks) {
  ##
  ## Generate bi-polar color ramp
  ##
  require(RColorBrewer); require(classInt)
  
  ## define breaks and color assignment
  q.neg.breaks <- classIntervals((varName[varName < 0]), n=neg.breaks, style="quantile")
  q.pos.breaks <- classIntervals((varName[varName > 0]), n=pos.breaks, style="quantile")
  qBreaks <- c(q.neg.breaks$brks[-(neg.breaks+1)],0,q.pos.breaks$brks[-1])     # combine neg and pos over zero
  
  pal.neg <- brewer.pal(neg.breaks, "Blues")
  pal.pos <- brewer.pal(pos.breaks, "Reds")
  colPal <- c(rev(pal.neg),pal.pos)                                                # combine palettes
  
  mapCol <- colPal[findInterval(varName,qBreaks,rightmost.closed=T)]
  return(list(mapCol=mapCol,qBreaks=qBreaks,colPal=colPal))
} # end:biPolarColorRamp

## Read the shape files in
gridfile <- "DEMPointsDisRiv"
riverfile <- "RiversRevised"
samplefile <- "SamplePts2019"

gridDEM <- rgdal::readOGR(dsn=getwd(), layer=gridfile, integer64="warn.loss")
(projUTM <- proj4string(gridDEM))
gridDEM <- cbind(gridDEM,coordinates(gridDEM))           # Add coordinates to df
summary(gridDEM)

sampleDEM <- rgdal::readOGR(dsn=getwd(), layer=samplefile, integer64="warn.loss")
proj4string(sampleDEM)                                   # requires re-projection
sampleDEM <- spTransform(sampleDEM, projUTM) 
sampleDEM <- cbind(sampleDEM,coordinates(sampleDEM))     # Add coordinates to df
summary(sampleDEM)

river <- rgdal::readOGR(dsn=getwd(), layer=riverfile, integer64="warn.loss")
proj4string(river)
summary(river)

## Transform coordinates
yMean <- mean(sampleDEM$coords.x2); xMean <- mean(sampleDEM$coords.x1)
ySd <- sd(sampleDEM$coords.x2); xSd <- sd(sampleDEM$coords.x1)

sampleDEM$X <- zTransCoord(sampleDEM$coords.x1,xMean,xSd)
sampleDEM$Y <- zTransCoord(sampleDEM$coords.x2,yMean,ySd)
gridDEM$X <- zTransCoord(gridDEM$coords.x1,xMean,xSd)
gridDEM$Y <- zTransCoord(gridDEM$coords.x2,yMean,ySd)

## Fit 1stOrder TrendSurface
polyForm1 <- makeTrendPolyForm(ELEVATION~RIVERDIST+I(RIVERDIST^2), ~X+Y, shpDf=sampleDEM, polyDeg=1)
polyForm1

lmTrend1 <- lm(polyForm1, data=sampleDEM)
summary(lmTrend1)

## Fit 2nd Order TrendSurface
polyForm2 <- makeTrendPolyForm(ELEVATION~RIVERDIST+I(RIVERDIST^2), ~X+Y, shpDf=sampleDEM, polyDeg=2)
polyForm2

lmTrend2 <- lm(polyForm2, data=sampleDEM)
summary(lmTrend2)
anova(lmTrend1,lmTrend2)

## Fit 3rd Order TrendSurface
polyForm3 <- makeTrendPolyForm(ELEVATION~RIVERDIST, ~X+Y, shpDf=sampleDEM, polyDeg=3)
polyForm3

lmTrend3 <- lm(polyForm3, data=sampleDEM)
summary(lmTrend3)
car::residualPlots(lmTrend3)
car::vif(lmTrend3)

## Test the models
anova(lmTrend2,lmTrend3)

## Fit 4th Order TrendSurface
polyForm4 <- makeTrendPolyForm(ELEVATION~RIVERDIST, ~X+Y, shpDf=sampleDEM, polyDeg=4)
polyForm4

lmTrend4 <- lm(polyForm4, data=sampleDEM)
summary(lmTrend4)
car::vif(lmTrend4)

## Test the models
anova(lmTrend3,lmTrend4)   # 4th order is overkill

## Perform Prediction
predTrend3 <- predict(lmTrend3, gridDEM, se.fit = TRUE)

## Map 3rd Order Trend Surface with co-variable
predFit <- predTrend3$fit
summary(predFit)
predHist <- hist(predFit,freq=F,main="Predicted DEM 3rd Order Trend")
breakptsPred <- predHist$breaks                                                     # Define Elevation classes
nclPred <- length(breakptsPred)-1                                                   # Number of classes
palPred <- terrain.colors(nclPred)                                                  # get colors
colsPred <- palPred[findInterval(predFit,
                                 breakptsPred,rightmost.closed=T)]                    # assign appropriate color to each residual

plot(gridDEM, axes=T, col=colsPred, pch=15,cex=1)                                   # Observed DEM Grid
plot(river,col="blue",add=T)
title("Predicted 3rd Order DEM Surface\nIncorporating Distance to Rivers")

## Map 3rd Order Residuals
lmTrend3Resid <- gridDEM$ELEVATION - predFit
hist(lmTrend3Resid)
biCol <- biPolarColorRamp(lmTrend3Resid,pos.breaks=4,neg.breaks=4)
plot(gridDEM, axes=T, col=biCol$mapCol, pch=15, cex=1)
plot(river,col="blue",add=T)
title("Residuals of 3rd Order DEM Surface\nIncorporating Distance to Rivers")


## Map 3rd Order Prediction Error Standard Deviation
predSe <- predTrend3$se.fit
seHist<- hist(predSe)
breakptsSe <- seHist$breaks                                                      # Define Prediction Error classes
nclSe <- length(breakptsSe)-1                                                    # Number of classes
palSe <- rev(heat.colors(nclSe))                                                 # get colors
colsSe <- palSe[findInterval(predSe, breakptsSe,rightmost.closed=T)]             # assign appropriate color to each residual

plot(gridDEM, axes=T, col=colsSe, pch=15,cex=1)                                  # Observed DEM Grid
plot(river,col="blue",add=T)
title("Prediction Standard Errors of 3rd Order DEM Surface\nIncorporating Distance to Rivers")

## Evaluate possible bias
obsElev <- gridDEM$ELEVATION
obsElevHist <- hist(obsElev, breaks=seq(325,950,by=25), freq=F, ylim=c(0,0.004), main="Observed Elevation")
lines(density(obsElev),col="red",lwd=2)

hist(predFit, breaks=obsElevHist$breaks, freq=F, ylim=c(0,0.004), main="Predicted Elevation")
lines(density(predFit),col="red",lwd=2)

## Root Mean Square Error
predError <- gridDEM$ELEVATION - predFit
(RMSETrend <- sqrt(sum(predError^2)/length(predError)))

summary(cbind(obsElev,predFit))
plot(obsElev,predFit,pch=".")  ## note granularity of observed elevation
title(main="Observed against Predicted Elevations")
abline(a=0,b=1)

## Evaluate spatial scale
boxDEM <- bbox(gridDEM)
(boxDEM[1,2]-boxDEM[1,1])  # x-coord range
(boxDEM[2,2]-boxDEM[2,1])  # y-coord range

##################
## Kriging Part ##
##################

## Model Variograms
vgm()  # vgm models in gstat
show.vgms()

sampleDEM$sampleRes <- residuals(lmTrend3)     # add residuals to sampleDEM

## Make gstat object for simple Kriging
gPred <- gstat(id="simKrig", formula=sampleRes~1, beta=0, data=sampleDEM)

## gstat object for universal Krigin
gPredUni <- gstat(id="uniKrig",formula=polyForm3, data=sampleDEM)

## Select final variogram for simple kriging of residuals
varioFinal <- variogram(gPred, cutoff=150000, width=2000) # alternative: variogram(sampleRes~1,sampleDEM)
plot(varioFinal)
varioFinal
  
varioFit <- fit.variogram(varioFinal,model=vgm(model="Mat",range=150000,nugget=50)) 
print(varioFit)
plot(varioFinal,model=varioFit)

## Explore instationary variogram
hscat(sampleRes~1, data=samplePoints, (0:8)*10000)         # explore correlation in distance bands

## Explore directional variogram
plot(variogram(gPred, map=T,cutoff=100000,width=5000)) # Radar plot
e1.vgm <- variogram(gPred, cutoff=80000, width=5000, alpha=c(0,45,90,135))  # directional variogram
plot(e1.vgm)

## Predict the residuals
gPred <- gstat(id="simKrig",gPred, model=varioFit)
predSimKrig <- predict(gPred, model=varioFit, newdata=gridDEM) # alternative: krige(sampleRes~1,sampleDEM, gridDEM, model=varioFit)

##
## Map spatially predicted residuals and standard error
##

## Predicted Kriging residuals
predRes <- predSimKrig$simKrig.pred      # Kriging cell residual prediction
hist(predRes)
colRamp <- biPolarColorRamp(predRes, pos.breaks=4)


plot(gridDEM, axes=T, col=colRamp$mapCol, pch=15, cex=1)
plot(river,col="blue",add=T)
plot(sampleDEM,col="green",add=T)
legend("topright", title="Residuals", legend=leglabs(round(colRamp$qBreaks,2)),
       fill=colRamp$colPal, bg="white",ncol=1)
title("Predicted Simple Kriging Residuals")

## Map kriging prediciton standard error
seRes <- sqrt(predSimKrig$simKrig.var)       # Standard errors of Kriging Residuals

seHist<- hist(seRes)
breakptsSe <- seHist$breaks
nclSe <- length(breakptsSe)-1
palSe <- rev(heat.colors(nclSe))
colsSe <- palSe[findInterval(seRes,
                             breakptsSe,rightmost.closed=T)]
plot(gridDEM, axes=T, col=colsSe, pch=15,cex=1)
plot(river,col="blue",add=T)
plot(sampleDEM,col="red",add=T)
legend("topright", title="Predicted DEM",legend=leglabs(round(breakptsSe,2)),
       fill=palSe,bg="white",ncol=1)
title("Standard Errors of Kriging Residuals")

##
## Map overall surfaces
##

fitTot <- predFit+predRes                          # Total predicted cell value
seTot <- sqrt(predSe^2+predSimKrig$simKrig.var)    # Total cell prediction standard error

## Map overall prediciton surface
fitHist<- hist(fitTot,freq=F)
lines(density(gridDEM$ELEVATION),col="red",lwd=2)
breakptsFit <- fitHist$breaks
nclFit <- length(breakptsFit)-1
palFit <- terrain.colors(nclFit)
colsFit <- palFit[findInterval(fitTot,
                               breakptsFit,rightmost.closed=T)]
plot(gridDEM, axes=T, col=colsFit, pch=15,cex=1)
plot(river,col="blue",add=T)
plot(sampleDEM,col="red",add=T)
legend("topright", title="Predicted DEM",legend=leglabs(round(breakptsFit,2)),
       fill=palFit,bg="white",ncol=1)
title("Prediction Surface: Trend (3rd)\nplus Kriging Residuals")

## Map overall prediciton standard error
seHist<- hist(seTot)
breakptsSe <- seHist$breaks
nclSe <- length(breakptsSe)-1
palSe <- rev(heat.colors(nclSe))
colsSe <- palSe[findInterval(seTot,
                             breakptsSe,rightmost.closed=T)]
plot(gridDEM, axes=T, col=colsSe, pch=15,cex=1)
plot(river,col="blue",add=T)
plot(sampleDEM,col="red",add=T)
legend("topright", title="Prediction STD. Error",legend=leglabs(round(breakptsSe,2)),
       fill=palSe,bg="white",ncol=1)
title("Prediction Standard Errors of 3rd Order DEM Surface\nplus Kriging Residuals")

## Evaluated Prediction Error
predError <- gridDEM$ELEVATION - fitTot
(RMSETotal <- sqrt(sum(predError^2)/length(predError)))

hist(predError)
colRamp <- biPolarColorRamp(predError,pos.breaks=6)
colsError <- colRamp$mapCol

plot(gridDEM, axes=T, col=colsError, pch=15,cex=1)
plot(river,col="blue",add=T)
plot(sampleDEM,col="green",add=T)
legend("topright", title="Prediction Error",legend=leglabs(round(colRamp$qBreaks,2)),
       fill=colRamp$colPal,bg="white",ncol=1)
title("Prediciton Error of 3rd Order Trends\nwith Kriging Residuals Model")

summary(pred.res)
summary(predError)

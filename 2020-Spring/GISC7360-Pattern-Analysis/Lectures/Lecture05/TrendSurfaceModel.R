library(maptools); library(RColorBrewer)
rm(list=ls(all=TRUE))
setwd("G:\\UTD_Classes\\2020Spring\\GISC7360_Pattern_Analysis\\Lecture05\\TrendShapeFiles")  # Directory with data files

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

## Read map data
landBnd <- rgdal::readOGR(dsn=getwd(), layer="Land", integer64="warn.loss")
proj4string(landBnd)                              # Current system
statPts <- rgdal::readOGR(dsn=getwd(), layer="BritStations", integer64="warn.loss")
proj4string(statPts)
elevGrid <- rgdal::readOGR(dsn=getwd(), layer="Elevation", integer64="warn.loss") 
proj4string(elevGrid)

## Review Shape-file properties
as.data.frame(statPts)       
( boxPred <- bbox(elevGrid) )
( boxCali <- bbox(statPts) )

## Map Study Area
plot(landBnd,axes=T,col=grey(0.9),border="green",
     xlim=boxPred[1,],ylim=boxPred[2,])
plot(statPts,col="red",add=T)
plot(elevGrid,col="black",pch=".",add=T)
box()

## Transform coordinates
yMean <- mean(statPts$LATITUDE1); xMean <- mean(statPts$LONGITUDE1)
ySd <- sd(statPts$LATITUDE1); xSd <- sd(statPts$LONGITUDE1)

# statPts$caliX <- zTransCoord(statPts$LONGITUDE1,xMean,xSd)
# statPts$caliY <- zTransCoord(statPts$LATITUDE1,yMean,ySd)
# elevGrid$caliX <- zTransCoord(elevGrid$LONGITUDE1,xMean,xSd)
# elevGrid$caliY <- zTransCoord(elevGrid$LATITUDE1,yMean,ySd)

# ## Test no transformation
statPts$caliX <- statPts$LONGITUDE1
statPts$caliY <- statPts$LATITUDE1
elevGrid$caliX <- elevGrid$LONGITUDE1
elevGrid$caliY <- elevGrid$LATITUDE1

## 1st Order Trendsurface
## Fit 1stOrder TrendSurface
polyForm1 <- makeTrendPolyForm(SEPTTEMP~ALT, coordForm =~caliX+caliY, shpDf=statPts, polyDeg=1)
polyForm1
lmTrend1 <- lm(polyForm1, data=statPts)
summary(lmTrend1)

## 2nd Order TrendSurface
polyForm2 <- makeTrendPolyForm(SEPTTEMP~ALT, ~caliX+caliY, shpDf=statPts, polyDeg=2)
lmTrend2 <- lm(polyForm2, data=statPts)
summary(lmTrend2)

## Perform partial F-test: Compare first and second order trendsurface
anova(lmTrend1, lmTrend2)

## 3rd Order TrendSurface
polyForm3 <- makeTrendPolyForm(SEPTTEMP~ALT, ~caliX+caliY, shpDf=statPts, polyDeg=3)
lmTrend3 <- lm(polyForm3, data=statPts)
summary(lmTrend3)
car::vif(lmTrend3)   # check multicollinearity

## Perform partial F-test: Compare second and third order trendsurface
anova(lmTrend2, lmTrend3)


## => Best model is 2nd order
## Perform Prediction
predTrend2 <- predict(lmTrend2, elevGrid, se.fit = TRUE)

## Map 2nd Order Trend Surface
predFit <- predTrend2$fit
n.col <- 9
pal <- brewer.pal(n.col,"YlOrRd")
predClass <- classInt::classIntervals(predFit, n.col, style="equal")
predClass
plot(predClass, pal=pal)
predCol <- classInt::findColours(predClass,pal)

plot(elevGrid,axes=T,col=predCol,pch=15,cex=2,
     xlim=boxPred[1,],ylim=boxPred[2,],
     main="Predition: 2nd Order Trendsurface")
plot(landBnd,axes=T,border="green",add=T)
plot(statPts,col="blue",add=T)
legend("topright", title = "Temperatur Predictions", 
       legend = leglabs(round(predClass$brks, digits = 0)), fill = pal, 
       bty = "o", ncol = 1)

## Map 2nd residuals
# Cannot be done because the prediction grid does not have observed temperature data

## Map 2nd Order Prediction Uncertainty
predSe <- predTrend2$se
n.col <- 9
pal <- brewer.pal(n.col,"Reds")
seClass <- classInt::classIntervals(predSe, n.col, style="equal")
seClass
plot(seClass,pal=pal)
seCol <- classInt::findColours(seClass,pal)

plot(elevGrid,axes=T,col=seCol,pch=15,cex=2,
     xlim=boxPred[1,],ylim=boxPred[2,],
     main="Standard Error: 2nd Order Trendsurface")
plot(landBnd,axes=T,border="green",add=T)
plot(statPts,col="blue",add=T)
legend("topright", title = "Prediction Std.-Erros", 
       legend = leglabs(round(seClass$brks, digits = 0)), fill = pal, 
       bty = "o", ncol = 1)



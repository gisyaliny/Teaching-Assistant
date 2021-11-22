rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
        dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

## ----setup, include=FALSE------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, prompt = TRUE, comment = "R>", 
                      cache = FALSE, fig.width=7.2)

library(car); library(maptools); library(spdep); library(TexMix); library(spatialreg)


## ----import--------------------------------------------------------------------------------------------------
setwd("E:\\Lectures2021\\GISC7310\\GLSHeteroSpaAC\\ItalyMaps")
##
## Read Poly Shapefiles (readShapePoly in library maptools)
##
getinfo.shape("Provinces.shp")
neig.shp <- rgdal::readOGR(dsn=getwd(), layer="Neighbors", integer64="warn.loss", stringsAsFactors=TRUE)
prov.shp <- rgdal::readOGR(dsn=getwd(), layer="Provinces", integer64="warn.loss", stringsAsFactors=TRUE)
summary(prov.shp)

proj4string(prov.shp)                                    # map projection
prov.centroid <- coordinates(prov.shp)                   # Get province centroids
prov.bbox <- bbox(prov.shp)                              # province bounding box for map region


## ----qualmap, fig.width=7, fig.height=9----------------------------------------------------------------------
is.factor(prov.shp$REGION)
table(prov.shp$REGION)

plot(neig.shp,axes=T,col=grey(0.9), border="white",      # background: neighboring countries
     xlim=prov.bbox[1,], ylim=prov.bbox[2,])                     
mapColorQual(prov.shp$REGION, prov.shp, map.title="Italy's Regions",
                          legend.title="Region", add.to.map=T)        



## ----gradmap, , fig.width=7, fig.height=9--------------------------------------------------------------------
plot(neig.shp,axes=T,col=grey(0.9),border="white",       # first background (axes=T adds lat/long frame)
     xlim=prov.bbox[1,],ylim=prov.bbox[2,])              # within bounding box
mapColorRamp(prov.shp$TOTFERTRAT, prov.shp, breaks=8,    # second add map
              map.title="Spatial Pattern of Fertility Rate",
              legend.title="Fertility Rate", 
              legend.cex=0.7, add.to.map=T)



## ----basicreg, fig.width=9, fig.height=9---------------------------------------------------------------------
## Regression model for fertility including model diagnostics
scatterplotMatrix(~TOTFERTRAT+ILLITERRAT+FEMMARAGE+DIVORCERAT+TELEPERFAM, data=prov.shp,
                   smooth=list(span = 0.35, lty.smooth=1, col.smooth="red", col.var="salmon"),
                   regLine=list(col="green"))

fert.lm <- lm(TOTFERTRAT~ILLITERRAT+FEMMARAGE+DIVORCERAT+TELEPERFAM, data=prov.shp)
summary(fert.lm,corr=F)
vif(fert.lm)

# Perform Residual Diagnostics
influenceIndexPlot(fert.lm, id=list(n=3,labels=prov.shp$PROVNAME))
fertResid <- residuals(fert.lm)


## ----outlier, fig.width=7, fig.height=9----------------------------------------------------------------------
## Why is Bolzano-Bozen an extreme observation? Shall we delete it?
idx.max <- which.max(abs(fertResid))        # Get index of a record with "outlying" observation 

## Map potential outlier
extremeObs <- rep(0, length(fertResid))
extremeObs[idx.max] <- 1
extremeObs <- factor(extremeObs, labels=c("Population","Outlier"))
table(extremeObs)
plot(neig.shp,axes=T,col=grey(0.9),border="white",                 # background: neighboring countries
     xlim=prov.bbox[1,],ylim=prov.bbox[2,])                     
mapColorQual(extremeObs, prov.shp, map.title="Potential Outlier",
              legend.title="Outliers", add.to.map=T)        

## Inspect outlier
prov.shp@data[idx.max,]                      # List info of record centre is outlier

## Delete outlier or update information
#prov.shp <- prov.shp[-idx.max ]             # Delete extreme observation from shapefile 
prov.shp@data[idx.max, "TOTFERTRAT"] <- 1.2  # Or change its value within the shapefile

## Continue with updated dataset
fert.lm <- lm(TOTFERTRAT~ILLITERRAT+FEMMARAGE+DIVORCERAT+TELEPERFAM,data=prov.shp) # update model
summary(fert.lm)
influenceIndexPlot(fert.lm, id=list(n=3, labels=prov.shp$PROVNAME))


## ----heterocheck---------------------------------------------------------------------------------------------
## Check for heteroscedasticity
fert.fgls <- lmHetero(TOTFERTRAT~ILLITERRAT+FEMMARAGE+DIVORCERAT+TELEPERFAM | log(FEMPOP94),
                      data=prov.shp)
summary(fert.fgls)


## ----wlm-----------------------------------------------------------------------------------------------------
fert.wlm <- lm(TOTFERTRAT~ILLITERRAT+FEMMARAGE+DIVORCERAT+TELEPERFAM, data=prov.shp,
               weights=fert.fgls$weights)


## ----residmap, fig.width=7, fig.height=9---------------------------------------------------------------------
## Plot Regression Residuals (Bi-polar)
fertResid <- weighted.residuals(fert.wlm)                # Update residuals
hist(fertResid, main="Residuals of Weighted Model")      # Explore distribution to
length(fertResid[fertResid < 0])                         # identify number of pos/neg classes
length(fertResid[fertResid >= 0])

plot(neig.shp,axes=T,col=grey(0.9),border="white",
     xlim=prov.bbox[1,],ylim=prov.bbox[2,])               # first background
mapBiPolar(fertResid, prov.shp,                           # second regression residuals
            neg.breaks=5, pos.breaks=5, break.value=0.0, 
            map.title="Fertility Model Residuals",
            legend.title="Residuals", 
            legend.cex=0.7, add.to.map=T)


## ----linkmap, fig.width=7, fig.height=9----------------------------------------------------------------------
## Plot Augmented Spatial Links among Italian Provinces
## Notes: Shape file has been edited so satellite islands are connected to mainland
## Alternatively spdep::edit.nb function (does not work with RStudio)
prov.link <- poly2nb(prov.shp, queen=F)                          # Generate neighbors links

plot(neig.shp,axes=T,col=grey(0.9),border="white",
     xlim=prov.bbox[1,],ylim=prov.bbox[2,])                      # First background
plot(prov.shp,col="palegreen3" ,border=grey(0.9), axes=T, add=T) # Second plot areas
plot(prov.link,coords=prov.centroid, pch=19, cex=0.1,            # Third plot links focused at centroids
     col="blue", add=T)
title("Augmented Spatial Links among Provinces")                 # Forth add title
box()                                                            # Fifth refresh frame


## ----morantest-----------------------------------------------------------------------------------------------
prov.linkW <- nb2listw(prov.link, style="W")                   # generated row-sum standardized neighbors list
spOutliers <- moran.plot(weighted.residuals(fert.wlm),         # Moran plot with outlier diagnositics
                         prov.linkW, labels=prov.shp$PROVNAME)          
lm.morantest(fert.wlm, prov.linkW)                             # Test with W-coding scheme


## ----SARmodel------------------------------------------------------------------------------------------------
fert.SAR <- spautolm(TOTFERTRAT~ILLITERRAT+FEMMARAGE+DIVORCERAT+TELEPERFAM, 
                     data=prov.shp, weights = fert.fgls$weights,
                     na.action="na.omit", listw=prov.linkW, family="SAR")
summary(fert.SAR)


## ----SARlikeratio--------------------------------------------------------------------------------------------
## Likelihood Ratio test (identical to LR from spautolm)
#(likeH0 <- logLik(fert.lm))                     # Use for unweighted model
(likeH0 <- fert.fgls$logLikeH1)
(likeH1 <- logLik(fert.SAR))

cat("chi-square value:  ", chi <- -2*(likeH0[1]-likeH1[1]))
cat("error-probability: ", pchisq(chi, df=1, lower.tail=F))


## ----SARresid, fig.width=7, fig.height=9---------------------------------------------------------------------
## Moran test applying randomization because ML may not be normal distributed
plot(neig.shp,axes=T,col=grey(0.9),border="white",
     xlim=prov.bbox[1,],ylim=prov.bbox[2,])                        # first background
mapBiPolar(residuals(fert.SAR), prov.shp,                          # second regression residuals
            neg.breaks=5, pos.breaks=4, break.value=0.0, 
            map.title="Fertility SAR Model Residuals",
            legend.title="Residuals", legend.cex = 0.6,
            add.to.map=T)

## Evaluate ML residuals for spatial autocorrelation
moran.mc(residuals(fert.SAR), prov.linkW, nsim=9999, alternative = "less") 


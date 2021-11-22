rm(list=ls())                        # Start with clean workspace
library(DallasTracts); library(car); library(rgdal)
setwd("E:\\Lectures2019\\GISC6301\\Chapter13\\ItalyMaps")

## Read polygons of neighboring countries
neig.shp <- readOGR(dsn=".",layer = "Neighbors", integer64 = "allow.loss")

## Read polygons of Italy provinces
italy.shp <- readOGR(dsn=".",layer = "Provinces", integer64 = "allow.loss")
italy.bbox <- bbox(italy.shp)                            # get bounding box

## Calculate population density
italy.shp$POPDEN <- italy.shp$TOTPOP94 / italy.shp$AREA

## add row.names sequence id. Notice syntax "italy.shp@data" to get to the attribute table
row.names(italy.shp@data) <- 1:length(italy.shp)

## Explore distribution of dependent variable
plot(neig.shp, axes=T, col=grey(0.9), border="white", 
     xlim=italy.bbox[1,], ylim=italy.bbox[2,]) 

mapColorRamp(italy.shp$TOTFERTRAT,italy.shp, breaks=7,       
             map.title="Italy Fertility Rate ",
             legend.title="Fertility Rate",add.to.map=T,
             legend.cex=0.9)

## Evaluate scatterplot matrix
scatterplotMatrix(~TOTFERTRAT+FEMMARAGE9+DIVORCERAT+ILLITERRAT+TELEPERFAM+POPDEN, data=italy.shp)

## Need to correct for positive skewness ???
scatterplotMatrix(~TOTFERTRAT+FEMMARAGE9+DIVORCERAT+ILLITERRAT+TELEPERFAM+log(POPDEN), data=italy.shp)

## Run multiple regression model
lm0 <- lm(TOTFERTRAT~FEMMARAGE9+DIVORCERAT+ILLITERRAT+TELEPERFAM+log(POPDEN), data=italy.shp)
summary(lm0)

## Updated model without population density
lm1 <- lm(TOTFERTRAT~FEMMARAGE9+DIVORCERAT+ILLITERRAT+TELEPERFAM, data=italy.shp)
summary(lm1)

##
## Basic model diagnostics
##
vif(lm1)                                                     # check for multicollinearity
qqPlot(lm1, main="Distribution of residuals")                # check for normality of residuals
residualPlots(lm1, main="Residuals against variables")       # check for non-linear relationships
influenceIndexPlot(lm1, id=TRUE)                             # detailed residual diagnositics

## Explore outlier
italy.shp@data[65, ]                                         # again, notice syntax @data

##
## Augment model by regions. Any substantial improvements???
##
lm2 <- lm(TOTFERTRAT~REGION+FEMMARAGE9+DIVORCERAT+ILLITERRAT+TELEPERFAM, data=italy.shp)
summary(lm2)

##
## Evaluate residuals for spatial pattern
##
resid.lm1 <- residuals(lm1)

sum(resid.lm1 < 0)                  # suggests 4 negative classes
sum(resid.lm1 >= 0)                 # suggests 5 positive classes

plot(neig.shp,axes=T,col=grey(0.9),border="white", 
     xlim=italy.bbox[1,], ylim=italy.bbox[2,]) 
mapBiPolar(resid.lm1, italy.shp, break.value=0, neg.breaks=4, pos.breaks=5,
           map.title="Residual Pattern of Fertility Model",
           legend.title="Residuals",legend.cex=0.9, add=TRUE)


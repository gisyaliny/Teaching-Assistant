library(sp)
rm(list=ls(all=TRUE))               # Clean objects from workspace

## Read shape-file
Shape <- rgdal::readOGR(dsn="G:\\UTD_Classes\\2020Spring\\GISC7360_Pattern_Analysis\\Lecture01\\RegionCityShape", 
                        layer="Region", integer64="warn.loss")
## Plot Polygons
polyCol <- c("orange1","orange4","orange3","orange2")
plot(Shape, border="grey", col=polyCol)
box()
title("Weighted Regional Means by City Size")

## Read Cities
City <- rgdal::readOGR(dsn="G:\\UTD_Classes\\2020Spring\\GISC7360_Pattern_Analysis\\Lecture01\\RegionCityShape", 
                        layer="Cities", integer64="warn.loss")
CityDf <- as.data.frame(City)                                       # get into dataframe
wgt <- (CityDf[,"POPULATION"]/max(CityDf[,"POPULATION"]))           # define circle area for mapping
wgt <- sqrt(wgt/pi)*6                                               # translate area into circle radius

n <- nrow(CityDf)                                                   # number of cities

points(City,pch=16,col="blue",cex=wgt)                              # map cities by their size
CityDf[,"REGION"] <- factor(CityDf[,"REGION"])                      # coerce into factor 

## Arithmetic means
MeanX <- tapply(CityDf[,"coords.x1"],CityDf[,"REGION"],mean,simplify=F)
MeanY <- tapply(CityDf[,"coords.x2"],CityDf[,"REGION"],mean,simplify=F)
points(MeanX,MeanY,col="blue",pch=7,cex=1.5)                        # Marked plain mean by circle

## Calculate weighted means of city coordinates in each region
regionSet <- unique(CityDf[,"REGION"])                              # get region IDs
for (idxReg in regionSet){                                          # cycle over regions
  City.id <- which(CityDf[,"REGION"] == idxReg)                     # get record IDs for particular region
  ## Weighted means (tapply does not work for function "weighted.mean)
  x.w.mean <- weighted.mean(CityDf[City.id,4],CityDf[City.id,"POPULATION"],na.rm=TRUE)
  y.w.mean <- weighted.mean(CityDf[City.id,5],CityDf[City.id,"POPULATION"],na.rm=TRUE)
  points(x.w.mean,y.w.mean,col="red",pch=11,cex=1.5)                   # Marked weighted mean by cross
}
legend(locator(1),
       legend=c("Weighted Mean","Plain Mean"),
       col=c("red","blue"),
       pch=c(11,7))


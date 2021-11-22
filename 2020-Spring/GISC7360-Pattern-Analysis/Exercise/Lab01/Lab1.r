rm(list=ls()) 
library(sp)
Shape <- rgdal::readOGR(dsn="./MyStudyArea/MyFourRegions.shp",integer64="warn.loss")
City <- rgdal::readOGR(dsn="./MyStudyArea/MyPopPlaces.shp",integer64="warn.loss")
CityDf <- as.data.frame(City) 


polyCol <- c("orange1","orange4","orange3","orange2")
plot(Shape, border="grey", col=polyCol)
box()

wgt <- (CityDf[,"POPULATION"]/max(CityDf[,"POPULATION"])) 
wgt <- sqrt(wgt/pi)*6 
n <- nrow(CityDf) 
points(City,pch=16,col="blue",cex=wgt)   

CityDf$REGIONID

## Arithmetic means
MeanX <- tapply(CityDf[,"coords.x1"],CityDf[,"REGIONID"],mean,simplify=F)
MeanY <- tapply(CityDf[,"coords.x2"],CityDf[,"REGIONID"],mean,simplify=F)
(cbind(MeanX,MeanY))
points(MeanX,MeanY,col="blue",pch=7,cex=1.5)    

regionSet <- unique(CityDf[,"REGIONID"])  

# for (idxReg in regionSet){                                          # cycle over regions
#   City.id <- which(CityDf[,"REGIONID"] == idxReg)                     # get record IDs for particular region
#   ## Weighted means (tapply does not work for function "weighted.mean)
#   x.w.mean <- weighted.mean(CityDf[City.id,4],CityDf[City.id,"POPULATION"],na.rm=TRUE)
#   y.w.mean <- weighted.mean(CityDf[City.id,5],CityDf[City.id,"POPULATION"],na.rm=TRUE)
#   points(x.w.mean,y.w.mean,col="red",pch=11,cex=1.5)                   # Marked weighted mean by cross
# }
library(foreach)

regionSet <- unique(CityDf[,"REGIONID"])
xy_ma <- matrix(NA,length(regionSet),2)
for(i in 1:length(regionSet)){
  idxReg <- regionSet[i]
  City.id <- which(CityDf[,"REGIONID"] == idxReg)
  x.w.mean <- weighted.mean(CityDf[City.id,4],CityDf[City.id,"POPULATION"],na.rm=TRUE)
  y.w.mean <- weighted.mean(CityDf[City.id,5],CityDf[City.id,"POPULATION"],na.rm=TRUE)
  xy_ma[i,] <-c(x.w.mean,y.w.mean)
  
}
xy_ma


df1 <- foreach::foreach (i = 1:length(regionSet),combind = rbind) %do% {
  idxReg <- regionSet[i]
  City.id <- which(CityDf[,"REGIONID"] == idxReg)                     
  ## Weighted means (tapply does not work for function "weighted.mean)
  x.w.mean <- weighted.mean(CityDf[City.id,4],CityDf[City.id,"POPULATION"],na.rm=TRUE)
  y.w.mean <- weighted.mean(CityDf[City.id,5],CityDf[City.id,"POPULATION"],na.rm=TRUE)
  points(x.w.mean,y.w.mean,col="red",pch=11,cex=1.5) 
  data.frame(x.w.mean,y.w.mean)
}

euclidMedian <- function(xy){
  ## Function that evaluates the cost at any point XY given
  ## the global vector argments: x, y, and w
  cost <- sum(w * sqrt( (x-xy[1])^2 + (y-xy[2])^2 ))
  return(cost)              
} 

nofPoints <- 10

## Assign a random Poisson weight
w <- rpois(nofPoints,lambda=1)+1    # To avoid zeros a 1 is added


tol <- 0.0001
maxIter <- 80
nofPoints <- 10


xy_eu <- matrix(NA,length(regionSet),2)

for(i in 1:length(regionSet)){
  iIter <- 0
  City.id <- which(CityDf[,"REGIONID"] == regionSet[i])
  w <- CityDf[City.id,"POPULATION"]
  x <- CityDf[City.id,4]
  y <- CityDf[City.id,5]
  xOld <- xy_ma[i,1]
  yOld <- xy_ma[i,2]
  repeat{
    d <- sqrt((x-xOld)^2+(y-yOld)^2)                       # distance equation in step 1
    xNew <- sum(w*x/d)/sum(w/d)                            # update equation in step 2
    yNew <- sum(w*y/d)/sum(w/d)                             
    xyDiff <- sqrt((xNew-xOld)^2+(yNew-yOld)^2)            # calculate tolerance score
    xOld <- xNew
    yOld <- yNew
    iIter <- iIter + 1
    print(xyDiff)
    if (xyDiff < tol | iIter > maxIter) break
    Sys.sleep(0.5)
  } #end::repeat
  xy_eu[i,] <- c(xNew,yNew)
}
xy_eu



df2 <- foreach::foreach (i = 1:length(regionSet),combind = rbind) %do% {
  idxReg <- regionSet[i]
  City.id <- which(CityDf[,"REGIONID"] == idxReg)
  w <- CityDf[City.id,"POPULATION"]
  ## Weighted means (tapply does not work for function "weighted.mean)
  x <- CityDf[City.id,4]
  y <- CityDf[City.id,5]
  xWMean <- sum(w*x)/sum(w)
  yWMean <- sum(w*y)/sum(w)
  iIter <- 0
  xOld <- xWMean
  yOld <- yWMean
  repeat{
    d <- sqrt((x-xOld)^2+(y-yOld)^2)                       # distance equation in step 1
    xNew <- sum(w*x/d)/sum(w/d)                            # update equation in step 2
    yNew <- sum(w*y/d)/sum(w/d)                             
    xyDiff <- sqrt((xNew-xOld)^2+(yNew-yOld)^2)            # calculate tolerance score
    xOld <- xNew
    yOld <- yNew
    iIter <- iIter + 1
    if (xyDiff < tol | iIter > maxIter) break
    Sys.sleep(0.5)
  } #end::repeat
  points(xNew,yNew,col="grey",pch=11,cex=1.5)
  data.frame(xNew,yNew)
}

library("dplyr")
lungDf$season %>% 
  group_by(lungDf$monthNum) %>% 
  filter(any(lungDf$monthNum <= 3))


boxplot(lungDeath~monthNum,data=lungDf, main="lung death against month",
        xlab="Month", ylab="Death")
lungDf$monthNum
summary( mod<- lm(lungDeath~monthNum,data=lungDf))



amplitudeSe <- car::deltaMethod(mod, paste0("sqrt(",cosStr,"^2+",sinStr,"^2)"))
yrs <- 6 
avg <- mean(lungDf$lungDeath)
t <- seq(0, (yrs*12)-1, by=1)

## Parameters of 1st oscilation
lambda1 <- 1/12               # frequency for annual cycle consisting of 12 month
amplitude1 <- 1000              # annual up- and down-swing around the average, here zero
phaseShift1 <- 2              # peak of the oscillation at the each phaseShift's observation

## Parameters of 2nd oscilation
lambda2 <- 1/6
amplitude2 <- 200
phaseShift2 <- 12

## Function of oscillating time series with random error term
yt <- avg + amplitude1 * cos(2*pi*lambda1*(t - phaseShift1)) + rnorm(length(t),0,1)
if (T) yt <- yt + amplitude2 * cos(2*pi*lambda2*(t - phaseShift2))

## Plot time series
plot(yt~t,type="b", lwd=2, xlim=c(0,yrs*12+1), xaxt="n",col = 'red')
lines(x = t,y = lungDf$lungDeath,lwd = 2 , col = 'blue')

yrs <- 6 
t <- seq(0, (yrs*12)-1, by=1)
lambda1 <- 1/12 
lambda2 <- 1/6
## Generate time-dependent cos and sin proxy variable
yCos1 <- cos(t*lambda1*2*pi)
ySin1 <- sin(t*lambda1*2*pi)

yCos2 <- cos(t*lambda2*2*pi)
ySin2 <- sin(t*lambda2*2*pi)

## Estimate model
yrs.lm <- lm(lungDf$lungDeath~lungDf$monthNum + yCos1 + ySin1 )
summary(yrs.lm)
library(MASS)
best.lm <- lm(lungDf$lungDeath~ yCos1 + ySin1 )
summary(best.lm)
step.model <- stepAIC(yrs.lm, direction = "both", 
                      trace = FALSE)
summary(step.model)

plot(x = t,y = lungDf$lungDeath,lwd = 2 ,type = "l", col = 'blue')
lines(x = t,y = predict(best.lm),lwd = 2,col = 'red')

library(foreign)
setwd('G:\\UTD_Classes\\2020Spring\\GISC7360_Pattern_Analysis\\Lab01')
carCrashDir <- read.dbf("CarCrashDirection.dbf")
morningRush <- carCrashDir[carCrashDir$time == "MorningRush",]
morningRushDir <- morningRush[morningRush$crashDir,]
library(circular)
morningRushDir[,2]
morningRushc <- circular(morningRushDir[,2], type="angles", units="degrees", template='geographics')
plot(morningRushc, cex=1.5, bin=720, stack=TRUE, sep=0.035, shrink=1.3)

rm(list=ls(all=TRUE))  # start clean

library(car)
Flight <- foreign::read.spss("E:/Lectures2019/GISC6301/Chapter04/Helicopter.sav", to.data.frame=TRUE)
colnames(Flight) <- tolower(colnames(Flight))
dim(Flight)
cor(Flight$flighttime,Flight$winglength)
scatterplot(flighttime~winglength, data=Flight)
scatterplot(flighttime~jitter(winglength) | experiment, data=Flight, smooth=F, regLine=F)
my.lm <- lm(flighttime~winglength, data=Flight)
summary(my.lm)

## Aggregate by student experiment
FlightByStudent <- aggregate(cbind(winglength,flighttime)~experiment, data=Flight, mean)
FlightByStudent <- FlightByStudent[order(FlightByStudent$winglength), ]
dim(FlightByStudent)
cor(FlightByStudent$winglength,FlightByStudent$flighttime)

## Aggregate by winglength
FlightByWing <- aggregate(flighttime~winglength, data=Flight, mean)
cor(FlightByWing$winglength, FlightByWing$flighttime)

## Original observations and different aggregation levels
plot(Flight$winglength,Flight$flighttime,col="orange",pch=16,  # Plot original observations
     xlab="Wing-Length", ylab="Flight-Time",
     main="Flight Experiment at Different Aggregation Levels")
abline(lm(flighttime~winglength, data=Flight),                 # add regression line for original data
       col="grey", lwd=2)    
points(FlightByStudent$winglength, FlightByStudent$flighttime, # Average flighttime by student (fixed winglength)
       col="red",pch=1,cex=2)
points(FlightByWing$winglength, FlightByWing$flighttime,       # Average flighttime by fixed winglength
       col="blue",pch=10,cex=2)




rm(list=ls())
setwd("E:/Lectures2020/WorkingWithR")
MyPower <- foreign::read.spss("DallasTempPower.sav", to.data.frame=TRUE)

## Calculate New Variable DiffTemp
MyPower$DiffTemp <- MyPower$MaxTemp - MyPower$MinTemp

## Class Data-Frame
class(MyPower)
summary(MyPower)

## Class lm
MyReg <- lm(MaxTemp ~ MinTemp, data=MyPower)
class(MyReg)
summary(MyReg)

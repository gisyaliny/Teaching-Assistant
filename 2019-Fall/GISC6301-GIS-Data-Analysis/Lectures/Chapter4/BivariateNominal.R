rm(list=ls(all=TRUE))                                                     # Start clean
library(foreign); library(ggplot2)
options(digits=1)                                                          # try rounding to 2 digits

###############################
## Practice table operations ##
###############################
( hh.car <- read.dbf("E:\\Lectures2019\\GISC6301\\Chapter04\\HHCar.dbf") ) # hh.car is an aggregated data.frame with factors
class(hh.car)
sapply(hh.car, is.factor)                                                  # check for factors

( hh.car.tab <- xtabs(Freq~cars+HHSize, data=hh.car) )                     # convert dataframe to table with factors as table dims
class(hh.car.tab)

(hh.car.df <- as.data.frame(hh.car.tab) )                                  # reverse: table to dataframe with "Freq" created

## Table margins and proportions
( addmargins(hh.car.tab) )                                                 # add surrounding sums
( prop.table(hh.car.tab) )                                                 # cell proportions from total

## Does number of cars determine the household size?
( hh.car.rowpro <- prop.table(hh.car.tab,1) )                              # row proportions 
( addmargins(hh.car.rowpro, 2) )                                           # add row sums

## Household size determines number of cars
( hh.car.colpro <- prop.table(hh.car.tab,2) )                              # column proportions
( addmargins(hh.car.colpro, 1) )                                           # add column sums


## Dodge plot of absolute counts
ggplot(data=hh.car.df, aes(x=HHSize, y=Freq, fill=cars)) +                    
       geom_bar(position="dodge", stat="identity")

## Spinogram
vcd::spine(xtabs(Freq~HHSize+cars, data=hh.car), main="Number of Cars by Household Size in %" )

## Stacked plot of absolute counts
ggplot(data=hh.car.df, aes(x=HHSize, y=Freq, fill=cars)) +
       geom_bar(position="stack", stat="identity")
                                      
## Stacked plot of percentages
hh.car.colpro <- prop.table(hh.car.tab,2)                                 # row proportions 
hh.car.colpro.df <- as.data.frame(hh.car.colpro)                          # convert table into data.frame. Note: Freq
hh.car.colpro.df$Percent <- hh.car.colpro.df$Freq*100                     # transform proportions into percent

ggplot(data=hh.car.colpro.df, aes(x=HHSize, y=Percent, fill=cars)) +
       geom_bar(position="stack", stat="identity")

##############################################
## List of 100 observations without factors ##
##############################################
( hh.car.lst <- read.dbf("E:\\Lectures2019\\GISC6301\\Chapter04\\HHCarList.dbf") )  # what is the structure ?  
hh.car.lst$cars <- factor(hh.car.lst$cars, 
                          labels=c("c:0","c:1","c:2","c:3"))                # convert numeric to factor with labels
hh.car.lst$HHSize <- factor(hh.car.lst$HHSize, 
                            labels=c("p:2","p:3","p:4","p:5"))              # convert numeric to factor with labels
head(hh.car.lst, n=15)

( hh.car.tab <- xtabs( ~ HHSize+cars, data=hh.car.lst) )                    # convert data-frame table. Note syntax 

#####################################################################
## Converts aggregated dataframe into individual records dataframe ##
#####################################################################
( hh.car.df <- as.data.frame(hh.car.tab) )                                   # convert table to data-frame
hh.car.lst <- as.data.frame(lapply(hh.car.df, 
                                   function(x) rep(x, hh.car.df$Freq)))      # expand to individual observations
hh.car.lst
########################################
## Cross-tabulations like SAS or SPSS ##
########################################
gmodels::CrossTable(hh.car.lst$HHSize, hh.car.lst$cars, chisq=T)             # see help at gmodels::CrossTable


#######################################################
## Recoding a metric variable into an ordered factor ##
#######################################################
income <- rlnorm(200, mean=10, sd=1)/1000 + 15                              # 200 random household incomes in $1000
hist(income, xlab="Household Income (in $1000)")

( income.fac <- cut(income,breaks=c(min(income)-1,40,90,max(income)+1)) )   # split into classes of type factor

income.ord <- ordered(income.fac,levels(income.fac),
                      labels=c("low","medium","high"))                      # make ordered factor and rename classes
( income.tab <- table(income.ord) )                                
barplot(income.tab)                                                         # barplot (unsorted because ordinal variable)


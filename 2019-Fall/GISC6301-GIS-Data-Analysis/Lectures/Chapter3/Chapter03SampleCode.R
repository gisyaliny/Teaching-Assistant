##
## Descriptive numerical statistics
##

BiModalityIndex <- function(x, show=TRUE){
  ########################################################
  ## Bimodality index. BM > 0.55 means multimodality    ##
  ## Input:  numeric data vector                        ##
  ## Output: skewness, kurtosis, bm-index               ##
  ########################################################
  
  if (!is.numeric(x)) stop("Input vector not numeric")
  n1 <- length(x)
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < n1) warning("There are some missing input values")
  if (n < 4) stop("At least 4 observations are needed for calculation")
  
  require(e1071)     # library e1071 required for kurtosis and skewness
  
  x.skew <- skewness(x)
  x.kurt <- kurtosis(x)
  x.bm <- (x.skew^2+1)/(x.kurt+(3*(n-1)^2)/((n-2)*(n-3)))
  if(show){
    BmResults <- c("Skew"=round(x.skew,3),"Kurt"=round(x.kurt,3),"BMIndex"=round(x.bm,3))
    print(BmResults)
  }
  return( list(skew=x.skew,kurt=x.kurt,bimod=x.bm) )
} # end::BiModalityIndex

n <- 500                       # number of random numbers
p <- q <- 1                    # try 1 for uniform; 0.5 for bimodoal; 10 for unimodal
randomBeta <- rbeta(n,p,q)     # Generate random numbers from beta-distribution      

BMIndex <- BiModalityIndex(randomBeta)$bimod         # Prints statistics by default

## Visualize the distributions
hist(randomBeta, breaks=50, cex.main=1.5,            # Plot distribution with BM index
     main=bquote( "Beta("*.(p)*","*.(q)*") with BM="*.(round(BMIndex,3))) )

boxplot(randomBeta, horizontal = T, notch=T)         # Horizontal box-plot with BM index
title(main=bquote( "Beta("*.(p)*","*.(q)*") with BM="*.(round(BMIndex,3))), cex.main=1.5 )
rug(jitter(randomBeta))

##
## Use Concord dataset to calculate statistics
##
setwd("E:\\Lectures2019\\GISC6301\\WorkingWithR")                   # Working directory with Concord1.sav
Concord <- foreign::read.spss("Concord1.sav", to.data.frame=TRUE)   # imports data into a data-frame
## Explore the distribution of Water consumption for 1979, 1980 and 1981                                                                         
boxplot(Concord[c("water79","water80","water81")],
        main="Water Consumption in 1979, 1980 and 1981")

## Central tendency
mean(Concord$water81)                                               # mean
mean(Concord$water81, trim=0.1)                                     # trimmed mean
median(Concord$water81)                                             # median
quantile(Concord$water81,1/4)                                       # first quartile
quantile(Concord$water81,3/4)                                       # third quartile
min(Concord$water81)                                                # smallest value
max(Concord$water81)                                                # largest value
fivenum(Concord$water81)

## Spread
range(Concord$water81)                                              # value range
IQR(Concord$water81)                                                # interquartile range
var(Concord$water81)                                                # empirical variance
sd(Concord$water81)                                                 # empirical standard deviation

## Compare coefficients of variation
sd(Concord$water79,na.rm = T)/mean(Concord$water79,na.rm = T)       # water79 has missing values
sd(Concord$water80)/mean(Concord$water80)
sd(Concord$water81)/mean(Concord$water81)

## Shape of distributions
e1071::skewness(Concord$water81)                                    # skewness in the library e1071 
e1071::kurtosis(Concord$water81)                                    # kurtosis in the library e1071
BiModalityIndex(Concord$water81)                                    # bimodality index

## 
## Finding records by index
## 
idx.min <- which.min(Concord$educat)                                # Index of lowest education
Concord[idx.min, ]

idx.max <- which.max(Concord$income)                                # Index of highest income
Concord[idx.max, ]

idx.05p <- which(Concord$income < quantile(Concord$income,0.05))    # Indices of 5% lowest income
Concord[idx.05p, ]

##
## Sort df by variable. Look also at data window in RStudio
##
ConcordSorted <- Concord[order(Concord$educat), ]   # Sort df by education

##
## Manipulate the Concord data frame to get qroup-wise statistics
##

## Groupwise boxplots
boxplot(income~retire, data=Concord, varwidth=T,
        main="Income Distribution by Retirement Status")

gMean <- aggregate(income~retire, Concord, mean)                   # Group means
gSd <- aggregate(income~retire, Concord, sd)                       # Group standard deviations
gLength <- aggregate(income~retire, Concord, length)               # Number of observations by group

gConcord <- data.frame(retire=gMean$retire,                        # merge into a df
                       incomeMean=gMean$income, incomeSd=gSd$income, incomeLength=gLength$income)
gConcord

## weighted mean of aggregated data-frame compared to regular mean in individual data-frame
weighted.mean(gConcord$incomeMean, gConcord$incomeLength)
mean(Concord$income)

##
## Merge two data-frame
##
ConcordMerged <- merge(Concord, gConcord, by.x="retire", by.y="retire")  # Index variables in quotation marks
head(ConcordMerged, n=10)                                                # first 10 observations
tail(ConcordMerged, n=10)                                                # last 10 observations


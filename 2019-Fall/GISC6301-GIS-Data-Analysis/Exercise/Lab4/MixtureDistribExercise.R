BiModalityIndex <- function(x, show=TRUE){
  ########################################################
  ## Bimodality index. BM > 0.55 means multimodality    ##
  ## Input:  numeric data vector                        ##
  ## Output: mean, sd, skewness, kurtosis, bm-index     ##
  ########################################################
  
  if (!is.numeric(x)) stop("Input vector not numeric")
  n1 <- length(x)
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < n1) warning("There are some missing input values")
  if (n < 4) stop("At least 4 observations are needed for calculation")
  
  x.mean <- mean(x)
  x.sd <- sd(x)
  x.skew <- e1071::skewness(x) # library e1071 required for kurtosis and skewness
  x.kurt <- e1071::kurtosis(x)
  x.bm <- (x.skew^2+1)/(x.kurt+(3*(n-1)^2)/((n-2)*(n-3)))
  if(show){
    BmResults <- c("Mean"=round(x.mean,3),"Std"=round(x.sd,3),
                   "Skew"=round(x.skew,3),"Kurt"=round(x.kurt,3),
                   "BMIndex"=round(x.bm,3))
    print(BmResults)
  }
  invisible( list(mean=x.mean,sd=x.sd,skew=x.skew,kurt=x.kurt,bimod=x.bm) )
} # end::BiModalityIndex


makeMixDist <- function(nCount, mixProp=1, mu1, sd1, mu2=mu1, sd2=sd1,shuffle=TRUE ){
#######################################################################
## Generates a mixture of two random normal distributed variables
##  
##  Inputs: nCount   Total number of observations
##          mixProp  Proportion of observation in the first group
##          mu1      Mean of the first group
##          sd1      Standard deviation of the first group
##          mu2      Mean of the second group
##          sd2      Standard deviation of the second group
##          shuffle  Mixed sequence of observation in both groups
#######################################################################
  if((mixProp < 0) | (mixProp > 1)) stop("Mixture proportion must be in the interval 0<=mixProp<=1")
  if(sd1 <= 0) stop("First standard deviation must be larger than 0")
  if(sd2 <= 0) stop("Second standard deviation must be larger than 0")
  
  x1 <- rnorm(nCount*mixProp, mu1, sd1)         # first normal parent distribution
  x2 <- rnorm(nCount*(1-mixProp), mu2, sd2)     # second normal parent distribution
  xLabel <- factor(c(rep("G1",length(x1)),rep("G2",length(x2))))
  x <- data.frame(rNorm=c(x1,x2),xGroup=xLabel)
  if(shuffle==TRUE){
    randSort <- runif(length(x1)+length(x2))
    x <- x[order(randSort), ]       # randomly mix both vectors
  }
  return(x) 
} # end::makeMixDist

mixHist <- function(x, myClass=NULL, myTitle="Name"){
  ## 
  ## Generates boxplots and histograms of a mixture of two groups
  ## Input:  x        data-frame with random variables rNorm 
  ##                  and group factor xGroup
  ##         myClass  bin definition for histogram
  ##         myTitle  title of the plots
  ##
  gMean <- tapply(x$rNorm, x$xGroup, mean)      # group means
  gSd <-tapply(x$rNorm, x$xGroup, sd)           # group sd
  gCount <- tapply(x$rNorm, x$xGroup, length)   # cases by group
  gStats <- data.frame(Groups=levels(x$xGroup), gMean=gMean, gSd=gSd,gCount=gCount)
  ## Generate proportion based normal density curves for both G1 and G2
  xRange <- range(x$rNorm)
  xSeq <- seq(xRange[1],xRange[2],length=1000)
  g1Density <- dnorm(xSeq,mean=gMean[1],sd=gSd[1])*gCount[1]/sum(gCount)
  g2Density <- dnorm(xSeq,mean=gMean[2],sd=gSd[2])*gCount[2]/sum(gCount)
  xDen <- hist(x$rNorm, nclass=myClass, plot=FALSE)$density
  yMax <- max(g1Density,g2Density,xDen)
 
  ## Plot group normal density and rescale by proportion in full dataset
  title <- paste("Histogram:\nMixture Distribution of",myTitle)
  hist(x$rNorm,probability=TRUE,ylim=c(0,yMax),col="grey",xlab="Mixed X", main=title)       #base histogram
  lines(xSeq,g1Density,col="red",lwd=2)
  lines(xSeq,g2Density,col="blue",lwd=2)
  rug(x[x$xGroup=="G1",1], col="red")
  rug(x[x$xGroup=="G2",1], col="blue")
  abline(h=0,lwd=2)
  
  ## Plot boxplots
  title <- paste("Joint Box-Plot:\nMixture Distribution of",myTitle)
  boxplot(x$rNorm, xlab="Mixed X", main=title, horizontal=T)      # joint box plot
  rug(x[x$xGroup=="G1",1], side=1,col="red")
  rug(x[x$xGroup=="G2",1], side=1, col="blue")  
  title <- paste("Group-wise Box-Plot:\nMixture Distribution of",myTitle)
  boxplot(x$rNorm~x$xGroup, xlab="Mixed X", main=title, horizontal=T, varwidth=TRUE)
  rug(x[x$xGroup=="G1",1], side=1,col="red")
  rug(x[x$xGroup=="G2",1], side=1, col="blue")  
  
  invisible(gStats)    # return statistics only if assigned to variable
} #end::mixHist

## Generate Mixture distribution of two normal parent distributions
x <- makeMixDist(1000, mixProp=0.5, mu1=-1, sd1=2, mu2=1, sd2=1)
BiModVal <- BiModalityIndex(x[,1],show=F)[[5]]

mixHist(x, myTitle="Test Model")     # visulize mixture distribution
cat("\n\nDescriptive Statistics of Joint Mixture Distribution:\n")
print(round( c("Mean:"= mean(x[,1]), "WinMean@10%"= mean(x[,1],trim=0.1), 
               "SD:"= sd(x[,1]), "Skew:"= e1071::skewness(x[,1]), "Kurt:"= e1071::kurtosis(x[,1]), 
               "BiModIdx:"= BiModVal ), 3))


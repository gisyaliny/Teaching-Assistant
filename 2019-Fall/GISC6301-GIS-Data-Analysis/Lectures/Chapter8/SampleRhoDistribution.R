rm(list=ls(all=TRUE))  # start clean
library(mvtnorm)                           # needed to generate random correlated data
library(hexbin)                            # for crowded scatterplots
yMaxHisto <- 4000                          # max height of histogram

##
## Parameters for simulation experiment
##
nPopPts <- 200000                          # Number of population X_1 and X_2 members
rhoPop <- 0.80                             # Population correlation coefficient
nSamplePts <- 64                           # Size of one sample set
nSamples <- 10000                          # Number of sample sets

## Generate Bi-variate standard NV observations
(cormat <- matrix(c(1,rhoPop,rhoPop,1), ncol=2))
nvPts <- mvtnorm::rmvnorm(n=nPopPts, mean=c(0,0), sigma=cormat)   # set of bivariate normal distributed random sample values

## Plot 2d-distributions
bins <- hexbin(nvPts[,1], nvPts[,2], xbins=50)
plot(bins, xlab=expression(X[1]), ylab=expression(X[2]),
     main=bquote(paste("Bivariate Normal Distribution with ",rho==.(rhoPop))))

## Get sampling distribution of correlation coefficient
sampleRho <- numeric(nSamples)                    # initialize vector of r-values for all samples
for (i in 1:nSamples){
  setSample <- sample(1:nPopPts,nSamplePts)       # sample index of point pair
  sampleRho[i] <- cor(nvPts[setSample,])[1,2]     # evaluate correlation of sampled point pair
} # end::for

## Evaluate sampling distribution
hist(sampleRho,breaks=seq(-1,1,0.02),xlab="Sample Correlation Coefficient",ylim=c(0,yMaxHisto),
     main=bquote(paste("Sampling Distribution of  ",hat(rho)," given population ",rho==.(rhoPop),
                       " and ",n==.(nSamplePts))))
abline(v=rhoPop,col="red",lty=2)

if (rhoPop == 0){                          # rho is t-distributed with df=n-2 under H0: rho=0.0
  rCritLow <- qt(0.025, df=nSamplePts-2, lower.tail=T) * sqrt((1-rhoPop)/(nSamplePts-2))
  rCritHi <- qt(0.975, df=nSamplePts-2, lower.tail=T) * sqrt((1-rhoPop)/(nSamplePts-2))
  rug(c(rCritLow,rCritHi),ticksize=-0.03,lwd=2,col="red")
}
## Transform sampling distribution to NV
eZ <- 0.5*log((1+rhoPop)/(1-rhoPop))
varZ <- nSamplePts-3
zSampleRho <- ((0.5*log((1+sampleRho)/(1-sampleRho)))-eZ)/(1/sqrt(varZ))

hist(zSampleRho,breaks=seq(-5,5,0.2),xlab="Fisher z-transformed Correlation Coefficient",ylim=c(0,yMaxHisto),
     main=bquote(paste("Fisher z-transformed Distribution of  ",hat(rho)," given population ",rho==.(rhoPop),
                       " and ",n==.(nSamplePts))))
abline(v=0,col="red",lty=2)


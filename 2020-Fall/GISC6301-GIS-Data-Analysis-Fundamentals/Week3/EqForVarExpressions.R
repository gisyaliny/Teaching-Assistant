rm(list=ls())              # clear workspace => start fresh
library(compiler)          # Explicitly attach the compiler library
enableJIT(0)               # disable precompiled functions
options(digits=6)          # number of digits shown in console

VarComExpress <- function(x){
  ## Computational expression of the sample variance
  x <- as.vector(x)
  n <- length(x)                               # number of elements
  meanLoop <- 0 ; squareLoop <- 0              # initialize summation variables
  ## single loop to get summation terms
  for (i in 1:n) {
    meanLoop <- meanLoop + x[i]                # get sum of x
    squareLoop <- squareLoop + x[i]^2          # get sum of x^2
  }
  xMean <- meanLoop/n
  xVar <- (squareLoop - n*xMean^2)/(n-1)       # calculate variance
  return(xVar)
} # end::VarComExpress

VarDefExpress <- function(x){
  ################################################
  ## Defintional expression of the sample variance
  ################################################
  n <- length(x)        # number of observations
  ## Initialize loop sums
  meanLoop <- 0         
  varLoop <- 0
  ## Calculate mean
  for (i in 1:n) { 
    meanLoop <- meanLoop + x[i] 
  }  
  xMean <- meanLoop/n
  ## Calculate variance
  for (i in 1:n) { 
    varLoop <- varLoop + (x[i]-xMean)^2 
  }
  xVar <- varLoop/(n-1)
  return(xVar)
} # end::VarDefExpress

## generate random vector
inputMean <- 1e8                              # try 1e2 to 1e8 for inputMean 
nCount <- 1e7                                 # number of observations
x <- rnorm(nCount, mean=inputMean, sd=1)      # generate random input vector with target sd=1                              
print(object.size(x), units="Mb")             # memory allocation for object x

## evaluate distribution of x
summary(x, digits=12)
hist(x, freq=FALSE)
lines(density(x), lwd=2, col="red")

## evaluate R's internal, definitional and computational variance functions
timeSys <- system.time(varSys <- var(x))[3]             # R's internal function
timeDef <- system.time(varDef <- VarDefExpress(x))[3]   # definitional expression
timeCom <- system.time(varCom <- VarComExpress(x))[3]   # computational expression

## Report formatted results
data.frame(Method=c("Sys:","Def:","Com:"),
           Var=c(varSys,varDef,varCom),
           Time=c(timeSys,timeDef,timeCom))

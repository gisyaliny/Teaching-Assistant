rm(list=ls(all=TRUE))         # start clean
twoOsc <- FALSE               # allow for two overlapping oscilations
atanFnc <- FALSE              # evaluate atan functions
ampPhase <- function(mod, cosStr, sinStr, period){
  #############################################################
  ## Objective: Calculates amplitude and phase shift based on
  ##            sin and cos terms. Evaluates standard errors
  ##            and confidence intervals by car::delta-method
  ## Input:
  ## mod        model object (lm or glm)
  ## cosStr     name of cos-term as string in the model object
  ## sinStr     name of sin-term as string in the model object
  ## period     numeric length of a full period in # of obs.
  #############################################################
  
  if ( !is.character(cosStr) ) stop("cosStr must be entered as string")
  if ( !is.character(sinStr) ) stop("sinStr must be entered as string")

  ## Standard error of amplitude
  amplitudeSe <- car::deltaMethod(mod, paste0("sqrt(",cosStr,"^2+",sinStr,"^2)"))
  
  ## Standard error of phase shift
  atanStr <- paste0("atan(",sinStr,"/",cosStr,")")

  if (mod$coefficients[cosStr] > 0 & mod$coefficients[sinStr] > 0)
    phaseSe <- car::deltaMethod(mod, paste0(atanStr,"*",period,"/(2*pi)"))
  if (mod$coefficients[cosStr] < 0)
    phaseSe <- car::deltaMethod(mod, paste0("(",atanStr,"+pi)*",period,"/(2*pi)"))
  if (mod$coefficients[cosStr] > 0 & mod$coefficients[sinStr] < 0)
    phaseSe <- car::deltaMethod(mod,paste0("(",atanStr,"+2*pi)*",period,"/(2*pi)"))

  fourierResult <- rbind(amplitudeSe,phaseSe)
  row.names(fourierResult) <- c("Amplitude:","Phase Shift:")
  return(fourierResult)
}

##
## Build a periodic time series to explore its properties
## Remember: cos(0)=1, cos(pi/2)= 0, cos(pi)=-1 and cos(3*pi/2)=0
##
yrs <- 10                     # number of years
t <- seq(0, (yrs*12)-1, by=1) # monthly time index. Note: Jan=0 and Dec=11
avg <- 2145                     # overall average

## Parameters of 1st oscilation
lambda1 <- 1/12               # frequency for annual cycle consisting of 12 month
amplitude1 <- 3891              # annual up- and down-swing around the average, here zero
phaseShift1 <- 2              # peak of the oscillation at the each phaseShift's observation

## Parameters of 2nd oscilation
lambda2 <- 1/6
amplitude2 <- 10
phaseShift2 <- 3

## Function of oscillating time series with random error term
yt <- avg + amplitude1 * cos(2*pi*lambda1*(t - phaseShift1)) + rnorm(length(t),0,1)
if (twoOsc) yt <- yt + amplitude2 * cos(2*pi*lambda2*(t - phaseShift2))

## Plot time series
plot(yt~t,type="b", lwd=2, xlim=c(0,yrs*12+1), xaxt="n")
axis(1, at=c(0,12,24,36,48,60,72,84,96,108,120))
abline(h=avg, v=phaseShift1, lwd=1)                            # reference line
abline(h=c(avg-amplitude1,avg+amplitude1), col="green", lty=5) # amplitude
abline(v=c(0,12,24,36,48,60,72,84,96,108,120), lty=2, col="red")                # mark January of each year

## Generate time-dependent cos and sin proxy variable
yCos1 <- cos(t*lambda1*2*pi)
ySin1 <- sin(t*lambda1*2*pi)

yCos2 <- cos(t*lambda2*2*pi)
ySin2 <- sin(t*lambda2*2*pi)

## Estimate model
yrs.lm <- lm(yt~yCos1+ySin1)
if (twoOsc) yrs.lm <- lm(yt~yCos1+ySin1+yCos2+ySin2)
summary(yrs.lm)
car::vif(yrs.lm)

## Calculate phaseShift and amplidute. Input has to be period length (i.e., 1/lambda)
round(ampPhase(yrs.lm, "yCos1", "ySin1", 12), 2)
if (twoOsc) round(ampPhase(yrs.lm, "yCos2", "ySin2", 1/lambda2), 2)

## Non-linear least squares estimation of first wave
ytDf <- data.frame(yt,t)
yrs.nls <- nls(yt~b0+b1*cos(2*pi*1/12*(t-phase)), data=ytDf,
               start=list(b0=60, b1=15, phase=8))
summary(yrs.nls)

if (atanFnc){
## Evaluation atan and atan2 functions
  r <- seq(0, 2*pi, length.out=1000)
  yCos <- cos(r)
  ySin <- sin(r)
  
  plot(r, atan(ySin/yCos), asp=1, pch=".", 
       main="'atan' function")
  abline(h=c(-pi/2,0,pi/2),v=c(0,pi,2*pi), lty=3)
  
  plot(r, atan2(ySin,yCos), xlim=c(0,2*pi), pch=".", asp=1, 
       main="'atan2' function")
  abline(h=c(-pi,0,pi),v=c(0,pi,2*pi), lty=3)
}

rm(list=ls())
library(circular)

##############################################################################
# Plotting ozone level against wind direction (cylindrical)
##############################################################################

ozone <- c(28.0,85.2,80.5,4.7,45.9,12.7,72.5,56.6,31.5,112.0,20.0,72.5,16.0,45.9,32.6,56.6,52.6,91.8,55.2)
winddeg <- c(327,91,88,305,344,270,67,21,281,8,204,86,333,18,57,6,11,27,84)

plot(winddeg,ozone,
     ylab = "Ozone level",
     xlab = "Wind direction (in degrees)",
     ylim = c(0,120),
     xlim = c(0,360),
     xaxp = c(0,360,4),
     yaxp = c(0,120,6),
     cex.axis = 1.3,
     cex.lab = 1.6,
     pch = 16, cex=1.5)

n <- length(ozone)
winddegShift <- winddeg
for (j in 1:n) { if (winddegShift[j] > 180) {winddegShift[j] <- winddegShift[j]-360} }

plot(winddegShift,ozone,
     ylab = "Ozone level",
     xlab = "Wind direction (in degrees)",
     ylim = c(0,120),
     xlim = c(-180,180),
     xaxp = c(-180,180,4),
     yaxp = c(0,120,6),
     cex.axis = 1.3,
     cex.lab = 1.6,
     pch = 16, cex=1.5)

##############################################################################
# Randomisation test for circular-linear independence
##############################################################################

R2xtCorrCoeff <- function(lvar, cvar) {
  rxc <- cor(lvar, cos(cvar)) ; rxs <- cor(lvar, sin(cvar))
  rcs <- cor(cos(cvar), sin(cvar))
  R2xtVal <- ((rxc*rxc)+(rxs*rxs)-(2*rxc*rxs*rcs))/(1-rcs*rcs)
  return(R2xtVal)
}

R2xtIndTestRand <- function(lvar, cvar, NR) {
  R2xtObs <- R2xtCorrCoeff(lvar, cvar)
  R2xtObs1 <- c(R2xtObs) ; nxtrm <- 1
  for (r in 1:NR) {
    lvarRand <- sample(lvar)
    R2xtRand <- R2xtCorrCoeff(lvarRand,cvar)
    R2xtObs1 <- c(R2xtObs1,R2xtRand)
    if (R2xtRand >= R2xtObs) {nxtrm <- nxtrm+1} 
  }
  pval <- nxtrm/(NR+1)
  hist(R2xtObs1,breaks = 25,freq = F)
  lines(density(R2xtObs1,bw = ),col = 'blue',lwd = 1)
  abline(v=R2xtObs,col="red")
  return(c(R2xtObs, pval))
}

windrad <- winddeg*2*pi/360
R2xtObs1 <- R2xtIndTestRand(ozone, windrad, 9999)
hist(R2xtObs1,breaks = 25,freq = F)
lines(density(R2xtObs1,bw = ),col = 'red',lwd = 1)

#################################################################################
# Example: Roosting orientations of birds in the morning and afternoon (toroidal)
#################################################################################

lmorn <- c(105,120,135,95,155,170,160,155,120,115)   # East orientation
laft <- c(205,210,235,245,260,255,240,245,210,200)   # West orientation
plot(lmorn,laft)

cmorn <- circular(lmorn, type="angles", units="degrees")
caft <- circular(laft, type="angles", units="degrees")
plot(cmorn, units="degrees", zero=pi/2, rotation="clock", type="n")
ticks.circular(circular(seq(0, (11/6)*pi, by=pi/6)), zero=pi/2, rotation="clock", tcl=0.075)
points(cmorn, zero=pi/2, rotation="clock", pch=16, col="darkgrey", next.points=-0.1)
points(caft, zero=pi/2, rotation="clock", pch=16, next.points=0.1)

##############################################################################
# Jammalamadaka--Sarma correlation coefficient 
# H0: Independenc of Directons - H1: Covariation of directions
##############################################################################

cor.circular(cmorn, caft, test=T)



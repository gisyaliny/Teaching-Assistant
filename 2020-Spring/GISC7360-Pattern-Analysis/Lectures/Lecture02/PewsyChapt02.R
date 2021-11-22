library(circular)
rm(list=ls())

# =====================================
# Linear plot
# =====================================

oldpar <- par(mai=c(0.85, 0.85, 0.05, 0.05), cex.axis=1.1, cex.lab=1.3)
plot(wind, pch=16, xlab="Observation number", ylab="Wind direction (in radians)")

# =====================================
# Linear histogram
# =====================================

par(mai=c(0.85, 0.85, 0.05, 0.05), cex.axis=1.1, cex.lab=1.3)
hist(wind, main="", xlab="Wind direction (radians)", ylab="Frequency", breaks=seq(from=0,to=2*pi,by=pi/8), col="grey", xlim=c(0,2*pi))

# =====================================
# Centred linear histogram
# =====================================

n <- length(wind) ; cutpoint <- 2*pi-(5*pi/8) ; windshift <- 0
for (j in 1:n) { if (wind[j] >= cutpoint) {windshift[j] <- wind[j]-2*pi} 
  else {windshift[j] <- wind[j]} }
hist(windshift, main="", xlab="Wind direction (radians)", ylab="Frequency", breaks=seq(from=-5*pi/8, to=2*pi-5*pi/8, by=pi/8), col="grey", xlim=c(-5*pi/8,2*pi-5*pi/8))

# ========================================================================================
# Wind data: Convert to circular data object
# ========================================================================================

windc <- circular(wind, type="angles", units="radians", template='geographics')
summary(windc)

# =====================================
# Raw circular data plot
# =====================================

par(mai=c(0, 0, 0, 0))
plot(windc, cex=1.5, bin=720, stack=TRUE, sep=0.035, shrink=1.3)
axis.circular(at=circular(seq(0, 7*pi/4,pi/4)), labels=c("E","NE","N","NW","W","SW","S","SE"),
              zero=pi/2, rotation='clock', cex=1.5)
ticks.circular(circular(seq(0,2*pi,pi/8)), zero=pi/2, rotation='clock', tcl=0.075)

# =====================================
# Raw circular data plot and rose diagram
# =====================================

plot(windc, cex=1.5, bin=720, stack=TRUE, sep=0.035, shrink=1.3)
axis.circular(at=circular(seq(0, 7*pi/4,pi/4)), labels=c("E","NE","N","NW","W","SW","S","SE"),
              zero=pi/2, rotation='clock', cex=1.5)
ticks.circular(circular(seq(0,2*pi,pi/8)), zero=pi/2, rotation='clock', tcl=0.075)
rose.diag(windc, bins=16, col="darkgrey", cex=1.5, prop=1.3, add=TRUE)

# =====================================
# Raw circular data plot, rose diagram and kernel density estimates
# =====================================

plot(windc, cex=1.1, bin=720, stack=TRUE, sep=0.035, shrink=1.8)
axis.circular(at=circular(seq(0, 7*pi/4,pi/4)), labels=c("E","NE","N","NW","W","SW","S","SE"), 
              zero=pi/2, rotation='clock', cex=1.1)
ticks.circular(circular(seq(0,2*pi,pi/8)), zero=pi/2, rotation='clock', tcl=0.075)
rose.diag(windc, bins = 16, col="darkgrey", cex=1.1, prop=1.3, add=TRUE)
lines(density.circular(windc, bw=75), lwd=2, lty=2)
lines(density.circular(windc, bw=40), lwd=2, lty=1)
lines(density.circular(windc, bw=10), lwd=2, lty=3)

plot(windc, cex=1.1, bin=720, stack=TRUE, sep=0.035, shrink=1.8)
axis.circular(at=circular(seq(0, 7*pi/4,pi/4)), labels=c("E","NE","N","NW","W","SW","S","SE"), 
              zero=pi/2, rotation='clock', cex=1.1)
ticks.circular(circular(seq(0,2*pi,pi/8)), zero=pi/2, rotation='clock', tcl=0.075)
rose.diag(windc, bins=16, col="darkgrey", cex=1.1, prop=1.3, add=TRUE)
lines(density.circular(windc, bw=40), lwd=2, lty=1)
rayleigh.test(windc)

# ========================================================================================
# Ant data plot
# ========================================================================================

plot(fisherB10c$set1, units="degrees", zero=pi/2, rotation="clock", pch=16, cex=1.5)
ticks.circular(circular(seq(0,(11/6)*pi,pi/6)), zero=pi/2, rotation ='clock', tcl=0.075)
points(fisherB10c$set2, zero=pi/2, rotation="clock", pch=16, col="darkgrey", next.points=-0.1, cex=1.5)
points(fisherB10c$set3, zero=pi/2, rotation="clock", pch=1, next.points=0.1, cex=1.5)



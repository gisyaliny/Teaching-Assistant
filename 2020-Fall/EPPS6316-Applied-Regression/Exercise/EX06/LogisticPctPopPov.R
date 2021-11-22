library(TexMix); library(sp); library(car); library(effects)
data(tractShp)
tractShp <- tractShp[tractShp$NIGHTPOP!=0, ]   # Remove 2 airport tracts with NA's
sp::summary(tractShp)

hist(tractShp$PCTPOPPOV, breaks=seq(0, 60, by=2.5))
mapColorRamp(tractShp$PCTPOPPOV, tractShp, breaks=9,
             map.title="Percent of Population below the Poverty Limits",
             legend.title="PCTPOPPOV")
sp::plot(lakesShp, col="skyblue", border="skyblue",add=T)
sp::plot(hwyShp, col="cornsilk3", lwd=3, add=T)

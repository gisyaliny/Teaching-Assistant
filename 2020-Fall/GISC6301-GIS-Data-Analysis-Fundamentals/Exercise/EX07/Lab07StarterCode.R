library(TexMix); library(car)
rm(list=ls(all=T))

data(tractShp)
tractShp <- tractShp[!is.na(tractShp$MEDVALHOME), ]           # Remove Love Field and DFW airport tracts
row.names(tractShp@data) <- 1:length(tractShp)            # Add row-numbers to attribute table

tractShp$BUYPOW <- tractShp$BUYPOW/tractShp$NIGHTPOP      # Convert to relative number

tractShp$POST1980 <- tractShp$PCTB1980+tractShp$PCTB1990+ # % homes built after 1980
                     tractShp$PCTB2000+tractShp$PCTB2010
tractShp$NEWOLD <- as.factor(ifelse(tractShp$POST1980 > 50, "new", "old"))

hist(tractShp$POST1980)
mapBiPolar(tractShp$MEDVALHOME, tractShp, break.value=mean(tractShp$MEDVALHOME),
           neg.breaks=6, pos.breaks=4, legend.cex = 0.7)
plot(lakesShp, col="skyblue", border="skyblue",add=T)
plot(hwyShp, col="cornsilk4", lwd=3, add=T)

# # table(tractShp$MEDVALHOME)
# summary(tractShp$MEDVALHOME[index])
# # summary(tractShp$MEDVALHOME)
# 
# index <- which(!is.na(tractShp$MEDVALHOME))
# tractShp
# mapBiPolar(tractShp$MEDVALHOME[index], tractShp[index,], break.value=5000,
#            neg.breaks=6, pos.breaks=4, legend.cex = 0.7)
# # mapBiPolar(tractShp$MEDVALHOME[-index], tractShp[-index,],break.value = 15000)

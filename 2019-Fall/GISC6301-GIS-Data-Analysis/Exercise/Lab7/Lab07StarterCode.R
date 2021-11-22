library(DallasTracts); library(car)
rm(list=ls(all=T))

data(tractShp)
tractShp <- tractShp[!is.na(tractShp$BUYPOW), ]           # Remove Love Field and DFW airport tracts
row.names(tractShp@data) <- 1:length(tractShp)            # Add row-numbers to attribute table

tractShp$BUYPOW <- tractShp$BUYPOW/tractShp$NIGHTPOP      # Convert to relative number

tractShp$POST1980 <- tractShp$PCTB1980+tractShp$PCTB1990+ # % homes built after 1980
                     tractShp$PCTB2000+tractShp$PCTB2010
tractShp$NEWOLD <- as.factor(ifelse(tractShp$POST1980 > 50, "new", "old"))

hist(tractShp$POST1980)
mapBiPolar(tractShp$POST1980, tractShp, break.value=50,
           neg.breaks=6, pos.breaks=4, legend.cex = 0.7,
           map.title="Proportiong of Homes Built After 1980",
           legend.title="Percent built\npost 1980")
plot(lakesShp, col="skyblue", border="skyblue",add=T)
plot(hwyShp, col="cornsilk4", lwd=3, add=T)


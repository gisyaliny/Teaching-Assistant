library(spatstat)        # Key library for spatial point pattern analysis 
library(smacpod)         # Relative risk kernel densities based on statstat
library(maptools)        # To open shapefiles
rm(list=ls(all=TRUE))    # Clean objects from workspace

setwd("G:\\UTD_Classes\\2020Spring\\GISC7360_Pattern_Analysis\\Lab03\\SystematicPatterns")

## Importing shape-files
getinfo.shape("StudyArea.shp")
bnd.shp <- rgdal::readOGR("StudyArea.shp")
getinfo.shape("RegClust.shp")
pts.shp <- rgdal::readOGR("RegClust.shp")

## Converting shape-files to ppp objects
projUTM <- CRS("+proj=utm +zone=30 +units=m")
pts <- spTransform(pts.shp,projUTM)
bnd <- spTransform(bnd.shp,projUTM)
pts <- as.ppp(pts)
pts$marks <- NULL
pts <- pts[as.owin(bnd)]
summary(pts)
plot(pts, main="RegClust")
axis(1);axis(2)

## standard L-function
plot(Lest(pts))
plot(envelope(pts, fun=Lest,rmax = 60000), main="L-function")

## centered L-function
Ls <- Lest(pts,rmax = 60000)
plot(Ls, . - r ~ r, main="Border Corrected L-r functions")
Lv <- envelope(pts, fun=Lest,rmax = 60000)
plot(Lv, .-r ~ r, main="L-r Envelope")


# profit <- function(investment,n_month,rate){
#   rate = rate / 100
#   profits <- c()
#   investment_origin <- investment
#   for (year in 1:n_month) {
#     principal <- investment * (1 + rate)
#     profits <- c(profits,principal)
#     investment <-  investment_origin + principal
#   }
#   profits <- data.frame(profits)
#   return(profits)
#   
# }
# (profit(130,48,1))

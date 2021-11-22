rm(list=ls(all=T))
library(gstat); library(sp)
elev <- read.csv("G:\\UTD_Classes\\2020Spring\\GISC7360_Pattern_Analysis\\Lecture05\\elev.csv", 
                 header = TRUE, sep = "," )
summary(elev)

## remove observations with missing elevations
elev <- na.omit(elev)                              

## Make distribution approx. symmetric
hist(elev$elev)
summary(car::powerTransform(elev$elev))
elev$bc.elev <- car::bcPower(elev$elev, -1.77)     # box-cox transformation
hist(elev$bc.elev)
elev$log.elev <- log(elev$elev)
hist(elev$log.elev)

## Convert data-frame to sp-object
coordinates(elev) <- ~x+y
summary(elev)

## spatial extent of study area
elev.range <- bbox(elev)

## use aspect ratio to determine number of grid cells in the x- and y-directions
(asp <- elev.range[ ,2] - elev.range[,1])

## Generate sp-grid for predicition
grd <- expand.grid(x=seq(from=elev.range[1,1], to=elev.range[1,2], length.out=(100*(asp[1]/asp[2]))),
                   y=seq(from=elev.range[2,1], to=elev.range[2,2], length.out=100))

## Convert data-frame to SpatialPoints layout
coordinates(grd) <- ~x+y

## Convert SpatialPoints to SpatialPixels
gridded(grd) <- TRUE
plot(grd)
points(elev, pch=16, col="red", cex=0.7)

## perform deterministic inverse distance weighted interpolation
## Note the trend in the surfaces
e.idw <- idw(elev~1, locations=elev, newdata=grd, idp=2.0)     # idp is smoothing parameter         
spplot(e.idw["var1.pred"], main="Elevation: Inverse Distance Weighted Estimate", 
       col.regions=terrain.colors(20),sp.layout = list(elev))
# points(elev, pch=16, col="red", cex=0.7)
?terrain.colors
## Explore instationary variogram
hscat(elev~1, data=elev, breaks=(0:9)*500)                     # explore correlation in distance bands

## Explore variogram
plot(variogram(elev~1, data=elev, cutoff=7000, width=200, cloud=T))     # Variogram cloud

plot(variogram(elev~1, data=elev, map=TRUE, cutoff=7000, width=200),    # Radar plot => anisotrophy
     threshold=10)

# direction
plot(variogram(elev~1, data=elev, cutoff=7000, width=200,               # Directional plot 
               alpha=c(0,45,90,135)))

(e0.vgm <- variogram(elev~1, elev, boundaries=seq(0,7000,by=200)))      # Bin means
plot(e0.vgm)

## Model Variogram
vgm()  # vgm models in gstat
show.vgms()
e0.fit <- fit.variogram(e0.vgm, model = vgm(psill=3500, model="Exp", range=7000, nugget=100))
e0.fit # cannot fit variogram to instationary cloud
plot(e0.vgm, model=e0.fit)

## Explore stationary variogram
e1.gstat <- gstat(id="elevTrend", formula=elev~x+y, data=elev)                 # Build gstat object with trend

plot(variogram(e1.gstat, map=TRUE, cutoff=7000, width=200), threshold=10)      # Radar plot => anisotrophy
(e1.vgm <- variogram(e1.gstat, cutoff=7000, width=200, alpha=c(0,45,90,135)))  # directional variogram
plot(e1.vgm)

## isotropic fitting
(e1.vgmiso <- variogram(e1.gstat, cutoff=7000, width=200))  # isotropic variogram
plot(e1.vgmiso)
e1.fit <- fit.variogram(e1.vgmiso, model = vgm(psill=3500, model="Sph", range=4000, nugget=500))
e1.fit
e1.fit <- fit.variogram(e1.vgmiso, model = vgm(psill=3500, model="Exp", range=4000, nugget=500))
e1.fit
plot(e1.vgmiso,model=e1.fit,as.table=TRUE)

# ## anisotropic fitting
# e1.fit <- fit.variogram(e1.vgm, model = vgm(psill=3500, model="Sph", range=4000, nugget=500, 
#                                             anis=c(45,2000/4000)))             # main direction, range proportion of minor direction
# e1.fit
# plot(e1.vgm,model=e1.fit,as.table=TRUE)

## Perform prediction
e1.gstat <- gstat(e1.gstat, id="elevTrend", model=e1.fit)
e1.pred <- predict(e1.gstat, newdata=grd)
str(e1.pred)

## Plot Predicted Elevations
pts <- list("sp.points", elev, pch=4, col="black", cex=0.5)
spplot(e1.pred, zcol="elevTrend.pred", col.regions=terrain.colors(20), cuts=19,
       sp.layout=list(pts), contour=TRUE, labels=FALSE, pretty=TRUE, col="brown",
       main="Kriging with 1st order trend surface")

## Plot Predicted Uncertainty       
spplot(e1.pred, zcol="elevTrend.var", col.regions=rev(heat.colors(100)), cuts=99,
       sp.layout=list(pts), main="Prediction Variance")


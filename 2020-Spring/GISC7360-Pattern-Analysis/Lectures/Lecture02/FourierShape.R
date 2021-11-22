rm(list=ls())
library(sp); library(Momocs)

## Read shape-file
Shape <- rgdal::readOGR(dsn="E:\\Lectures2020\\GISC7360\\Lecture01", 
                        layer="BowTiePolyShape", integer64="warn.loss")

coord <- Shape@polygons[[1]]@Polygons[[1]]@coords         # polygon is closed

##
## Fourier Shape Analysis
##
nComponents <- 5                           # number of Fourier components
ef  <- efourier(coord, nComponents)        # calculate Fourier transformation
str(ef)
efi <- efourier_i(ef, nb.pts=240)          # recreate shape based on inverse Fourier transformation at nb.pts
coo_plot(coord)                            # draw original shape and its centroid
coo_draw(efi, border="orange", col=NA)     # overlay recreated Fourier shape and center of first harmonic


## Evaluate how many harmonic components are necessary
if (nComponents > 1){
  plot(cumsum(harm_pow(ef)[-1]), type="o",   # Evaluate fit 
       main="Cumulated harmonic power without the first harmonic",
       ylab="Cumulated harmonic power", xlab="Harmonic rank")
}


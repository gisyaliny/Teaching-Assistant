library(spatstat)
rm(list=ls(all=TRUE))

# HomoCSR <- foreign::read.dbf("G:\\UTD_Classes\\2020Spring\\GISC7360_Pattern_Analysis\\Lab03\\HomoCSR.dbf")

HomoCSR <- foreign::read.dbf("G:\\UTD_Classes\\2020Spring\\GISC7360_Pattern_Analysis\\Lab03\\CSRClust.dbf")

pat <- ppp(HomoCSR$x, HomoCSR$y, xrange=c(0,16), yrange=c(0,16))


spatialScale <- function(cell){
  plot(pat, cols="red", main=paste("Pattern with",cell,'x',cell,'Cell Counts'))
  qc16 <- quadratcount(pat, nx=cell, ny=cell)
  plot(qc16, add=T)
  vqc16 <- as.vector(qc16)
  cat(paste("VMR",cell,":"),var(vqc16)/mean(vqc16))
  quadrat.test(qc16)
}


spatialScale(2)

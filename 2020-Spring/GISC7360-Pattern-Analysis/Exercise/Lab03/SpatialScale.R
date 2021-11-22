library(spatstat)
rm(list=ls(all=TRUE))
setwd("M:\\Lectures2017\\GISC7360\\Labs\\Lab02")
HomoCSR <- foreign::read.dbf("HomoCSR.dbf")
hc <- ppp(HomoCSR$x, HomoCSR$y, xrange=c(0,16), yrange=c(0,16))
## 16 x 16 cells
plot(hc)
qc16 <- quadratcount(hc, nx=16, ny=16)
plot(qc16, add=T)
vqc16 <- as.vector(qc16)
cat("VMR 16: ",var(vqc16)/mean(vqc16))
quadrat.test(qc16)

## 8x8 cells
plot(hc)
qc8 <- quadratcount(hc, nx=8, ny=8)
plot(qc8, add=T)
vqc8 <- as.vector(qc8)
cat("VMR 8: ",var(vqc8)/mean(vqc8))
quadrat.test(qc8)

## 4x4 cells
plot(hc)
qc4 <- quadratcount(hc, nx=4, ny=4)
plot(qc4, add=T)
vqc4 <- as.vector(qc4)
cat("VMR 4: ",var(vqc4)/mean(vqc4))
quadrat.test(qc4)

## 2x2 cells
plot(hc)
qc2 <- quadratcount(hc, nx=2, ny=2)
plot(qc2, add=T)
vqc2 <- as.vector(qc2)
cat("VMR 2: ",var(vqc2)/mean(vqc2))
quadrat.test(qc2)

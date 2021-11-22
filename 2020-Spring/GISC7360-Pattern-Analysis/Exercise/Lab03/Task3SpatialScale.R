library(spatstat)
rm(list=ls(all=TRUE))
HomoCSR <- foreign::read.dbf("E:\\Lectures2020\\GISC7360\\Labs\\Lab03\\CSRClust.dbf")
pat <- ppp(HomoCSR$x, HomoCSR$y, xrange=c(0,16), yrange=c(0,16))

## 16 x 16 cells
plot(pat, cols="red", main="Pattern with 16x16 Cell Counts")
qc16 <- quadratcount(pat, nx=16, ny=16)
plot(qc16, add=T)
vqc16 <- as.vector(qc16)
cat("VMR 16: ",var(vqc16)/mean(vqc16))
quadrat.test(qc16)

## 8 x 8 cells
plot(pat, cols="red", main="Pattern with 8x8 Cell Counts")
qc8 <- quadratcount(pat, nx=8, ny=8)
plot(qc8, add=T)
vqc8 <- as.vector(qc8)
cat("VMR 8: ",var(vqc8)/mean(vqc8))
quadrat.test(qc8)

## 4 x 4 cells
plot(pat, cols="red", main="Pattern with 4x4 Cell Counts")
qc4 <- quadratcount(pat, nx=4, ny=4)
plot(qc4, add=T)
vqc4 <- as.vector(qc4)
cat("VMR 4: ",var(vqc4)/mean(vqc4))
quadrat.test(qc4)

## 2 x 2 cells
plot(pat, cols="red", main="Pattern with 2x2 Cell Counts")
qc2 <- quadratcount(pat, nx=2, ny=2)
plot(qc2, add=T)
vqc2 <- as.vector(qc2)
cat("VMR 2: ",var(vqc2)/mean(vqc2))
quadrat.test(qc2)

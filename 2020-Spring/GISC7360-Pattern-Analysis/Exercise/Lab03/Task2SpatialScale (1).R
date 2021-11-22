library(spatstat)
rm(list=ls(all=TRUE))
HomoCSR <- foreign::read.dbf("E:\\Lectures2020\\GISC7360\\Labs\\Lab03\\HomoCSR.dbf")
pat <- ppp(HomoCSR$x, HomoCSR$y, xrange=c(0,16), yrange=c(0,16))

## 16 x 16 cells
plot(pat, cols="red", main="Pattern with 16x16 Cell Counts")
qc16 <- quadratcount(pat, nx=16, ny=16)
plot(qc16, add=T)
vqc16 <- as.vector(qc16)
cat("VMR 16: ",var(vqc16)/mean(vqc16))
quadrat.test(qc16)


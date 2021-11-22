RNGversion("3.5.3"); set.seed(12345)

bw<-0.1
fx <- density(0.0, bw=bw, kernel="gaussian")
plot(fx)
str(fx)
sum(fx$y)*(fx$x[2]-fx$x[1])

n <- 10
x<-runif(n)
kern <- density(x, bw=bw, kernel="gaussian")
plot(kern, ylab="Intensity", main="")
points(x, rep(0, n), pch=20)
for(i in 1:n) {
  density_i <- density(x[i], bw=bw, kernel="gaussian")
  density_i$y <- density_i$y/n
  lines(density_i, lty=2)
}
legend("topright", legend=c("Cummulative Intensity", "Kernel Around Points"), lty=c(1,2))


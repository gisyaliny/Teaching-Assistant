##
## Demonstration of the central limit theorem
##
rm(list=ls())
set.seed(NULL)     # reset the seed to system clock status
#set.seed(pi)      # fixed sequence of random numbers

lab.pop <- c("Population 1","Population 2","Population 3")   # population names
pop.size <- 100000                                           # population size
n.sample <- 5000                                             # number of samples with mean.size
size <- c(4,16,256)                                          # number of obs for the mean
x <- seq(0,1,length=1001)

Beta <- data.frame(POP1=rbeta(pop.size,0.2,0.2),             # three population distributions
                   POP2=rbeta(pop.size,1,1),
                   POP3=rbeta(pop.size,1,5))

means <- matrix(NA,nrow=n.sample,ncol=3*4)                   # initialize matrix for results

for (i in 1:n.sample){
  idx <- sample(1:pop.size, size=size[1], replace=F)         # 4 obs per mean
  means[i,1:3] <- colMeans(Beta[idx,])
  idx <- sample(1:pop.size, size=size[2], replace=F)         # 16 obs per mean
  means[i,4:6] <- colMeans(Beta[idx,])
  idx <- sample(1:pop.size, size=size[3], replace=F)         # 256 obs per mean
  means[i,7:9] <- colMeans(Beta[idx,])

}

opar <- par(mfcol=c(4,3))                                    # define window for plots
  for(i in 1:3){                                             # Cycle over parent distributions and show their histograms
    xClass <- seq(0,1,,by=0.01)
    hist(Beta[,i],breaks=xClass,xlim=c(0,1),freq=F,ylab=expression(italic(f(x))),
         xlab=bquote(list(mu==.(round(mean(Beta[,i]),3)),sigma==.(round(sd(Beta[,i]),4)))),
         main=lab.pop[i])
    abline(h=0,lwd=2,col="gray")
    for(j in 1:3){                                             # Histograms of mean-distribution by size
      average <- mean(means[ ,(j-1)*3+i])
      stderror <- sd(means[ ,(j-1)*3+i])
      hist(means[, (j-1)*3+i], breaks=xClass, freq=F, main=bquote(n==.(size[j])),
           ylab=expression(italic(f(bar(x)))),
           xlab=bquote("Mean= "*.(round(average,3))*" , Standard Error= "*.(round(stderror,4))))
      abline(h=0,lwd=2,col="gray")
      lines(x,dnorm(x,average,stderror),col="red")
    } #end::for mean distributions
  }# end::for populaitons
par(opar)

rm(list=ls(all=TRUE))                                           # start clean
library(mvtnorm)                                                # multivariate normal distribution
library(scatterplot3d)                                          # fancy 3d scatterplots
library(ggplot2)
setwd("E:\\Lectures2018\\GISC6301\\Chapter05")
########################
## Discrete Variables ##
########################

fname.tab <- file.choose()                                      # Get file "HHCar.dbf"
( hh.car <- foreign::read.dbf(fname.tab) )                      # hh.car is an aggregated data.frame with factors

## Plot a Table 3-d in a scatterplot
scatterplot3d(hh.car, type = "h", lwd = 9, pch = " ",color="blue",
              x.ticklabs = c("c:0","","c:1","","c:2","","c:3"), 
              y.ticklabs = c("h:2","","h:3","","h:4","","h:5"),
              main = "Frequencies Table of Cars by HHSize")


## Joint Probabilities
( hh.car.tab <- xtabs(freq~cars+HHSize, data=hh.car) )          # convert to table with factors as table dims

## Conditional Probabilities
addmargins(hh.car.tab)                                           # Freqency table
round(addmargins(prop.table(hh.car.tab)),2)                      # Relative Frequency table
round(addmargins(prop.table(hh.car.tab,2),1),2)                  # column percentages

## Stacked plot using barplot
hh.car.mat <- as.matrix(prop.table(hh.car.tab,2))                # for stacked barplot must be matrix
barplot(hh.car.mat,beside=F, ylim=c(0,1),col=rainbow(4),
        xlab="HHSize",main="Conditional Relative Frequency of Cars given Household Size")
legend(locator(1),rownames(hh.car.mat),fill=rainbow(4),          # Add legend interactively
       bg="white",title="#ofCars")

## Stacked plot using ggplot aggregated records
hh.car.percent <- prop.table(hh.car.tab,2)                       # Calculate into "Freq" cars% within HHSize
hh.car.percent <- as.data.frame(hh.car.percent)                  # convert into data.frame
hh.car.percent$Percent <- hh.car.percent$Freq*100                # relative freq to %
names(hh.car.percent) <- c("cars","HHSize","Percent")            # add variable name            
# Alternatively: Do a group-wise transform() splitting by HHsize
#hh.car.percent <- plyr::ddply(hh.car, "HHSize", transform, Percent=freq/sum(freq)*100)

g.a <- ggplot(data=hh.car.percent, aes(x=HHSize, y=Percent, fill=cars))
g.a + geom_bar(position="stack", stat="identity")

## Stacked plot using ggplot with individual records
hh.car.indiv <- as.data.frame(lapply(hh.car,                     # Unpack into individual records
                              function(x) rep(x, hh.car$freq)))
p.i <- ggplot(data=hh.car.indiv, aes(x=HHSize, fill=cars))
p.i + geom_bar(position="stack")                                 # stacked example
p.i + geom_bar(position="dodge")                                 # dodge
p.i + geom_bar(position="fill")                                  # relative frequency

##########################
## Continuous Variables ##
##########################

## Plot Bi-variate Normal Distribution . You can experiment with these parameters
mu.1 <- 2; mu.2 <- 2
std.1 <- 2; std.2 <- 2                                         
rho.12 <- -0.9

x1 <- seq(mu.1-3*std.1, mu.1+3*std.1, length = 51)
x2 <- seq(mu.2-3*std.2, mu.2+3*std.2, length = 51)

dens <- matrix(dmvnorm(expand.grid(x1, x2), mean= c(mu.1,mu.2),   # Make Joint Density
               sigma = rbind(c(std.1^2, rho.12*std.1*std.2),      # Covariance Matrix
                             c(rho.12*std.1*std.2, std.2^2))), 
               ncol = length(x1),nrow = length(x2))

## 3-D plot. Set up empty frame
s3d <- scatterplot3d(x1, x2, seq(min(dens), max(dens), length = length(x1)),
                     type = "n", grid = FALSE, angle = 70,
                     zlab = expression(f(x[1], x[2])),
                     xlab = expression(x[1]), ylab = expression(x[2]))
title(main = "3-d Plot: Bivariate Normal Distribution",
      sub=bquote(list(mu[1]==.(mu.1),mu[2]==.(mu.2),
      sigma[1]==.(std.1),sigma[2]==.(std.2),
      rho[12]==.(rho.12))))

# add conditional density lines
for(i in length(x1):1) s3d$points3d(rep(x1[i], length(x2)), x2, dens[i,], type = "l")
for(i in length(x2):1) s3d$points3d(x1, rep(x2[i], length(x1)), dens[,i], type = "l")

## contour plot
image(x1, x2, dens, col=rev(heat.colors(12, alpha = 0.6)),
      xlab=expression(X[1]),ylab=expression(X[2]),main="")
title(main="Contour Plot: Bivariate Normal Distribution",
      sub=bquote(list(mu[1]==.(mu.1),mu[2]==.(mu.2),
      sigma[1]==.(std.1),sigma[2]==.(std.2),
      rho[12]==.(rho.12))))

contour(x1,x2,dens,nlevels = 12,add=T)
abline(v=mu.1,h=mu.2,lty=2,col="red") 

nd1 <- dnorm(x1,mean=mu.1,sd=std.1)*8 + min(x2)
lines(x1,nd1,lty=1, col="blue")
nd2 <- dnorm(x2,mean=mu.2,sd=std.2)*8 + min(x1)
lines(nd2,x2,lty=1, col="blue")


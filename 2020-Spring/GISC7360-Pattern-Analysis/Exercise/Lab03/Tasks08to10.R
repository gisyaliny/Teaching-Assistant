library(spatstat)
library(smacpod)
rm(list=ls(all=TRUE))    # Clean objects from workspace

data("okblack", package = "splancs")
data("okwhite", package = "splancs")

ppblack <- data.frame(x=okblack$x,y=okblack$y,mark="black")
ppwhite <- data.frame(x=okwhite$x,y=okwhite$y,mark="white")
ppboth <- rbind(ppblack,ppwhite)
range(ppboth$x);range(ppboth$y)

ppOffense <- with(ppboth, ppp(x,y,c(100,400),c(50,350), marks=mark))
summary(ppOffense)
plot(ppOffense);axis(1);axis(2)

## subset white and blacks
ppWhite <- subset(ppOffense, marks==levels(ppOffense$marks)[2])
ppBlack <- subset(ppOffense, marks==levels(ppOffense$marks)[1])

## Generate white intensity surface
( bw <- bw.ppl(ppWhite) )
imWhite <- density(ppWhite, 20)   
integral(imWhite)

plot(imWhite, main="104 White Offenders", col=rev(heat.colors(20)))
plot(ppWhite, add=T)
axis(1);axis(2)

## hypothetical black intensity surface
lambda <- imWhite/integral(imWhite)*147
plot(lambda, main="147 Black Offenders", col=rev(heat.colors(20)))
plot(ppBlack, add=T)
axis(1);axis(2)

## Task 08: Inhomogenous L function
LvInhom <- envelope(ppBlack, Linhom, use.theory = T, global= T,    # Density adjusted simulation
                    simulate=expression(rpoispp(lambda))) 
plot(LvInhom, .-r~r, main="Theoretical Density Adjusted Inhomogeneous Envelope", ylim=c(-200,200))

## Task 09: Difference of K-Functions. See Bailey&Gatrell p 130
LvDiffK <- kdest(ppOffense, case=2, level=0.95, nsim=99)
plot(LvDiffK, main="Black-White Difference K-functions")

## Task 10: Cross K-function. See Bailey&Gatrell p 122
LvCrossK <- envelope(ppOffense, Kcross, from=levels(ppOffense$marks)[2], to=levels(ppOffense$marks)[1])
plot(LvCrossK, main="Black-White Cross-type K-function")

library(circular)

## Generate uniform data and create several rose diagrams.
x <- circular(runif(400, 0, 2*pi))
rose.diag(x, bins = 36, prop=2, main = 'Uniform Distributed Directions')
points(x,stack=T)
summary(x)
deg(mean(x))                # convert mean to degrees
rho.circular(x); var(x)     # average length and variance
rayleigh.test(x)            # test for uniformity


## Generate von Mises data and create several rose diagrams.
myKappa <- 16
x <- rvonmises(n=400, mu=circular(pi), kappa=myKappa)    # 0<kappa<Inf. The closer kappa to zero the more uniform
rose.diag(x, bins=36, shrink=1.5, prop=1)
points(x,  stack=TRUE)
text(-0.9,1,paste("kappa= ",round(myKappa,2)))
summary(x)
deg(mean(x))
rho.circular(x); var(x)
rayleigh.test(x)

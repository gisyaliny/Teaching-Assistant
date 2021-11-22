##
## foreach parallel test in combination with MOR BLAS
## Open the Task Manager's CPU utilization to monitor CPU load
## Also check increase in memory utilization when running parallel
##
rm(list=ls(all=TRUE))

library(doParallel); library(foreach)
getDoParWorkers()               # Default number of used cores in %dopar%
nofThreats <- getMKLthreads()   # Default number of BLAS threads

n <- 4000
doAlot <- function(n){
  # Enter libraries need by doAlot here because SNOW needs one instance per thread
  mat <- matrix(runif(n^2), nrow = n, ncol = n)
  mat <- mat %*% t(mat)
  return(sum(mat))
}

iter <- 1:16

## No parallel execution, but Intel BLAS on
st1 <- system.time(
         result <- foreach(i=iter) %do% {
         doAlot(n) }
)

## Turn doParallel to 8 cores
cl <- makeCluster(nofThreats)
registerDoParallel(cl)  # May trigger firewall warning in Windows
getDoParWorkers()       # Default number of used cores in %dopar%
getDoParName()          # Default parallel interface: Shoud be SNOW for Windows

## Test BLAS interference by turning if on/off
## Parallel execution, Intel BLAS on
st2 <- system.time(
         result <- foreach(i=iter) %dopar% {
         doAlot(n) }
)

## Parallel execution, Intel BLAS off
setMKLthreads(1)               # Turn BLAS off, allow foreach to do all parallel work
st3 <- system.time(
         result <- foreach(i=iter) %dopar% {
         doAlot(n) }
)

stopCluster(cl)            # Turn clusters off
setMKLthreads(nofThreats)  # allow all Threads again for BLAS

## Time results
st1; st2; st3


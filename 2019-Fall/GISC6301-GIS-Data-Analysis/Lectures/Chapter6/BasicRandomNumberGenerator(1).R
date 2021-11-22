##
## BBR p 265 random number generator for uniform distribution
## Warning: Not for professional use !!!!
##          Use R's substantially enhenced capabilities
##

n <- 500                        # number of desired random values
x <- array(NA, dim=n)           # vector storing the random numbers

modulus <- 10000                # modulus (length of sequence)
seed <- as.numeric(Sys.time())  # Arbitrary seed value
a <- 1234                       # Arbitrary constant
b <- 5678                       # Arbitrary constant

x[1] <- (a+b*seed) %% modulus   # remainder of devision 6%%3=0, 7%%3=1 but 8%%3=2
for (i in 2:n){
  x[i] <- (a+b*x[i-1]) %% modulus
}
##
## Diagnositics
##
x <- x/modulus                # rescale to range U(0,1)
length(unique(x))             # check for dublicated values

hist(x, breaks = seq(0,1,by=0.05)); 
abline(h=25, col="red", lty=5)
ks.test(x,"punif",0, 1)       # is sequence uniform?
acf(x,lag.max = 100)          # is sequence random?

##
## Convert to random standard normal value
##
nv <- qnorm(x)   # map probabilities between 0..1 to normal x values
hist(nv)

##
## Using R's internal random number generator
##
RNGversion("3.5.2")      # The algorithm differs by the R-version
useSeed <- FALSE
if (useSeed)
  set.seed(12345) else   # Gets a sequence of reproducible random numbers
  set.seed(NULL)         # Let the computer pick the seed based on its current time
rNum <- runif(n)
hist(rNum, breaks = seq(0,1,by=0.05))
abline(h=25, col="red", lty=5)



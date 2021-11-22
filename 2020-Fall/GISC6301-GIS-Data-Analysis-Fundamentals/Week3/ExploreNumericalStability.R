rm(list=ls())
##
## Binary representation of numbers and joining operations
##
options(digits=18) # number of shown digits

## Infinity
10^308
10^308 * 2

## Machine epsilon: Smallest increment between two numbers
## Devide eps by 2 until 1+eps==1
eps <- 1
while ((1 + eps/2) != 1) { eps <- eps/2 }  # " != " means not equal
cat("Machine Epsilon:", eps)               # equal to 2^(-52)

## Sequence of expression evaluations IMPORTANT
## Note: "L" in numL forces num to integer
(11L/10L - 1L)*10L - 1L                # Conversion from integer to real in 11/10
11L/10L*10L - 1L*10L - 1L              # All integer arithmetic (multiplying 10 into expression)

## Sequence of input numbers IMPORTANT
x <- rnorm(100000)
xSorted <- sort(x)          
cat( "sum(x): ", sum(x) )              
cat( "sum(xSoredt): ", sum(xSorted) )
cat( "Difference: ", sum(x) - sum(xSorted)) # different number of scale shifts during addition 

options(digits=6) # reset to six default digits

rm(list=ls())
##
## for loop syntax
##

## calculate the sum of values ranging from 1 to 10
mySum1 <- 0 
for (i in 1:100) { mySum1 <- mySum1+i }
cat("Sum of values: ", mySum1)
cat("Analytical Expression:", (1+100)*100/2)

myNum <- c(3,2,8,3,6,9,4,2,6,8,5,8,1,4)
mySum2 <- mySum3 <- 0
for (i in 1:length(myNum)) { mySum2 <- mySum2 + myNum[i]}
for (i in seq_along(myNum)) { mySum3 <- mySum3 + myNum[i]}   # Perferred specification
cat("Sum of values using mySum2: ", mySum2,"\nand using mySum3: ", mySum3)

catNames <- c("Charlie","Gretchen","Austin")
catAges <- c(20,11,10)
for (i in seq_along(catNames)) {cat("\n",catNames[i],"is",catAges[i],"years old.")}

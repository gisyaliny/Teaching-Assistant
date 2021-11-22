##
## Examples of logical operations and if-statments
##

myT <- TRUE
myF <- FALSE

myT == myF    # comparison
myT == !myF   # comparison with negation

myT & myF     # Logical AND: Both sides must be TRUE
myT & !myF
myF & myF

myT | myF     # Logical OR: Just one side needs to be TRUE
myT | !myF
!myT | myF

myNum <- c(1,2,3,NA,4)
is.na(myNum)
myNum[!is.na(myNum)]

if (length(myNum) < 10) {cat("Not enought values")}  # Expression T

if (length(myNum) < 3) {cat("Not enought values")}   # Expression F => skip command

myLogical <- T
if (myLogical == T) {
    cat("myLogical is TRUE") } else {
    cat("myLogical is FALSE")
  }

myLogical <- F
if (myLogical == T) {
    cat("myLogical is TRUE") } else {
    cat("myLogical is FALSE")
  }


myProdFct <- function(x){
  ###########################################
  ## Input: numeric vector x               ##
  ## Output: product and # of valid data   ##
  ###########################################
  
  if (!is.numeric(x)) stop("Input vector not numeric")
  x <- x[!is.na(x)]      # remove any missing values
  n <- length(x)
  if (n < 2) stop("At least 2 factors are needed")
  
  x.prod <- 1            # initialize with neutral factor
  for (i in 1:n) {
    x.prod <- x.prod * x[i]
  } #end::for
  return( c(x.prod, n) )  # returns a vector
} # end:: myProdFct

v <- c(1,3,-1,NA,-4)
myProdFct(v)          # Test with clean data
myProdFct(v)[1]       # Just the product
myProdFct(v)[2]       # Number of valid factors

myProdFct(v[1])       # Test insufficient number of factors

myLet <- c("q",3,4,1) # Incorrect data input type
myProdFct(myLet)

prod(v)               # internal R function "prod"
prod(v, na.rm=TRUE)

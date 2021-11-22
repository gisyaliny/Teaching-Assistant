rm(list=ls(all=TRUE))                                # start clean
##
## Set Operations
##
(omega <- LETTERS[1:15])
(A <- sample(omega,5,replace=F))                     # Sample 5 objects without replacement
(B <- sample(omega,8,replace=T))                     # Sample 8 objects with replacement
(C <- LETTERS[1:4])
union(A,B)
intersect(A,B)
setdiff(omega,C)                                      # complement
is.element(C,omega)                                   # label elements in super set
C %in% omega                                          # alternative writing
all(is.element(C,omega))                              # subset TRUE
all(is.element(c(C,"Z"),omega))                       # subset FALSE

##
## Combinations
##
(n <- length(C))
choose(n,2)                                           # number of combinations
combn(C,2)                                            # Which 6 combinations with length 2 
choose(n,3)
combn(C,3)

##
## Permutations
##
choose(n,2) * factorial(2)                            # number of permutations
expand.grid(C,C)                                      # alterative function (however with replacement)

choose(n,3) * factorial(3)                            # number of permutations
expand.grid(C,C,C)                                    # alterative function (however with replacement)


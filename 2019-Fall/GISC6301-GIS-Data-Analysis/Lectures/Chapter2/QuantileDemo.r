## Data vector or 12 elements with ties in 2 elements in 4.4
x <- c(4.0,4.4,3.8,2.5,3.1,4.3,5.1,4.6,3.9,4.8,4.4,4.1)

## uniformly distributed numbers, randomly sorted
#x <- sample(1:20, size=20, replace = FALSE)  


( sort(x) )                                  # works only on vectors

## Display the data
boxplot(x, horizontal=TRUE)
rug(jitter(x), side=1, lwd=2, col="red")

## Re-ordering works on matrices and data-frames
idx <- order(x)                              # Order generates a shuffle index
                                             # take the idx's element and place
                                             # it at its position
xOrder <- x[idx]                             # Shuffle data positions in vector
(data.frame(x, idx, x[idx]))

## Ranking data
xRank <- rank(x, ties.method="random")       # Explore other methods
(cbind(x,xRank))

## quantiles Q[i](p) = (1 - z)*x[j] + z*x[j+1] with 0 <= z <= 1
( quantile(x[idx], prob=seq(0.1,0.9,by=0.1)) ) 
( quantile(x[idx], prob=c(0.25,0.5,0.75), type=2) )   # Quartiles
fivenum(x)

## Percentage (probability) points - for tied data use the larger percentile
( data.frame("X-value"=x[idx], "Percentile a=0.0"=ppoints(x,a=0.0),
             "Percentile a=0.5"=ppoints(x,a=0.5), "Percentile a=1.0"=ppoints(x,a=1.0)) )

## Empirical Distribution Function
summary(ecdf(x))
plot(ecdf(x))

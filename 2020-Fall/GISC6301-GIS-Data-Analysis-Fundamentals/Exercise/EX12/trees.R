## 
## Tree point pattern analysis
##

## Mapping trees point pattern
trees <- read.table("trees.txt", header=T)
plot(y~x, data=trees, col="blue", pch=16, asp=1)
abline(v=seq(0,1,0.1), h=seq(0,1,0.1), col="gray")
xt <- cut(trees$x, breaks=seq(0,1,0.1))
yt <- cut(trees$y, breaks=seq(0,1,0.1))
( cellCounts <- as.vector(table(xt,yt)) )

## VMR and test
( VMR <- var(cellCounts)/mean(cellCounts) )
( tValue <- (VMR-1)/sqrt(2/(length(cellCounts)-1)) )
( pval <- 2*pt(tValue, df=length(cellCounts)-1, lower.tail = FALSE) )

## Compare Against Poisson distribution
lambda <- mean(cellCounts)
( pptab <- cbind(table(cellCounts), 
                 dpois(0:8, lambda = lambda)*length(cellCounts)) )


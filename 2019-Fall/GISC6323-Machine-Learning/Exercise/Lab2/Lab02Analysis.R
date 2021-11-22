library(DallasTracts); library(ClustGeo)
rm(list=ls(all=T))

AdjOrder<- function(B){
  ##
  ## Generate higher order spatial link matrix and accumulate the order
  ##
  ## Input: B binary symmetric first order link matrix
  B <- as.matrix(B)
  n <- nrow(B)
  H <- B
  BB <- B
  for (i in 2:n){
    BB<- BB %*% B                        # power link matrix
    S <- ((BB>0) & (H==0))               # logical matrix: first time i-steps apart
    H <- H + i*S                         # label H cells by i-steps apart         
    if (min(BB) > 0 ) break              # reached diameter of of graph
  } #end::for
  diag(H)<- 0 
  return(H)
} #end::AdjOrder

clusterStats <- function(df, class, func=mean, scaleDf=TRUE, plotMeans=TRUE){
  ## 
  ## Input:  df         original metric variables
  ##         class      cluster membership of observations by cutree(tree, K)
  ##         func       function to be evaluated in each cluster
  ##         plotMeans  function to plot means of variables by cluster
  ##
  ## Return: data-frame with values of variables in each cluster
  ##
  require(ggplot2); require(tidyr)
  if(scaleDf) {df <- scale(df)}
  df <- data.frame(Cluster=class, df)
  nofcol <- ncol(df)
  nofclass <- length(levels(class))
  resDf <- df[1:nofclass, ]
  for (j in 2:nofcol){
    res <- aggregate(df[, j], list(class), func) 
    resDf[, j] <- res[1:nofclass, 2]
  } 
  resDf["Cluster"] <- as.factor(paste("Cluster",1:nofclass, sep=" "))
  
  if (plotMeans){
    centers <- resDf[,-1]
    centers <- as.data.frame(t(centers))
    names(centers) <- paste("Cluster",1:nofclass, sep=" ")
    centers$Symbol <- row.names(centers)
    centers <- tidyr::gather(centers, "Cluster", "Mean", -Symbol)
    centers$Color <- ifelse(centers$Mean > 0, "red", "blue")
    meansPlot <- ggplot(centers, aes(x=Symbol, y=Mean, fill=Color)) +
      geom_bar(stat='identity', position='identity', width=0.75) +
      scale_fill_manual(values=c("#91bfdb","#fc8d59"), guide=FALSE)+
      geom_hline(yintercept = 0)+
      facet_grid(Cluster ~ ., scales='free_y')+
      ggtitle("Cluster Statistics")+
      theme(plot.title = element_text(size=rel(2),face="bold",hjust=0.5))+
      ylab(paste("Function: ", deparse(substitute(func))))+
      xlab("Original Variables")
    plot(meansPlot)
  }
  invisible(resDf)
} ## end:clusterStats

layoutBoxPlots <- function(xVars, groups, ncol=4, zTrans=FALSE, varwidth=FALSE){
  ## 
  ## Generates side-by-side boxplots in a layout matrix
  ## for metric variables broken by a factor
  ##
  namesVars <- names(xVars)
  if (zTrans) xVars <- as.data.frame(scale(xVars))
  nVars <- length(xVars)
  nrow <- nVars%/%ncol+1
  layout(matrix(c(1:(nrow*ncol)), nrow=nrow, ncol=ncol, byrow=T))
  for (i in 1:nVars){
    boxplot(xVars[,i]~groups, varwidth=varwidth, xlab="Group Label", ylab=namesVars[i],
            main=paste("Feature:", namesVars[i]))
    abline(h=mean(xVars[,i]), lty=5, col="red")
  }
  layout(1)
} ## end::layoutBoxPlots

data(tractShp)
tractShp <- tractShp[!is.na(tractShp$BUYPOW), ]         # Remove 2 airport tracts with NA's

##
## Generate geographical distance matrices
##
nb <- spdep::poly2nb(tractShp, queen=F)                 # extract first order neighbors links
B <- spdep::nb2mat(nb, style="B")                       # convert neighbor list to binary matrix
stepDist <- as.dist(AdjOrder(B))                        # distance matrix of tracts i-steps apart
max(stepDist)                                           # maximum steps distance
sphDist <- as.dist(sp::spDists(tractShp, longlat=T))    # spherical distance matrix among tracts in km
## Convert adjacency matrix to dissimilarity
adjaDist <- 1-B; 
diag(adjaDist) <- 0; 
adjaDist <- as.dist(adjaDist)
## Select geographic distance metrix
geoDist <- adjaDist

##
## Visulize first order neighbors
##
plot(tractShp, col="palegreen3", border=grey(0.9), axes=T) 
plot(nb, coords=coordinates(tractShp), pch=19, cex=0.1, col="blue", add=T)
title("Spatial Neighbors Links among Tracts") 

##
## Select Variables for Dissimilarity organized in three groups:
##   [a] demographic features
##   [b] socio-economic features
##   [c] housing infrastructure features
##
tractShp$PRE1960 <- tractShp$PCTB1950+tractShp$PCTB1940+tractShp$PCTBPRE
tractShp$POST2000 <- tractShp$PCTB2000+tractShp$PCTB2010
tractShp$NIGHTDENS <- tractShp$NIGHTPOP/tractShp$AREA


varKeep <- c("PCTWHITE","PCTBLACK","PCTHISPAN","MEDAGE",
             "PCTUNIVDEG","HHMEDINC","PCTUNEMP","PCTNOHINS",
             "POPDEN","PCTDAYPOP","PCTHUVAC","MEDVALHOME","PRE1960","POST2000")
xVars <- tractShp@data
xVars <- xVars[varKeep]
summary(xVars)

## Substitute missing values in MEDVALHOME
xVars <- DMwR::knnImputation(xVars, k=5, scale=T, meth="weighAve")
summary(xVars)

## ID reach row of xVars
row.names(xVars) <- 1:nrow(xVars)

##
## Performing a principal Component Analysis on features
##
xPC <-prcomp(xVars, retx=TRUE, scale.=TRUE)
apply(xPC$x, 2, var)              # CAUTION: variance equal to eigenvalue

## Scree plot
plot(xPC$sdev^2, type="b", main="Scree Plot", 
     xlab="Component Order", ylab="Eigenvalue")
abline(h=1, v=1, lty=5)
nofComp <- 4                      # Number of selected components

## Explained Variance
cumVar <- cumsum(xPC$sdev^2)/length(xPC$sdev)*100
plot(cumVar, main="Percent Explained Variance",
     type="b", xlab="Component Order", ylab="Relative Cummulative Variance")
abline(v=nofComp, h=cumVar[nofComp], lty=5)

##
## Calculate feature distance matrix
##
featDist <- dist(scale(xPC$x[ , 1:nofComp]))
featDist <- dist(scale(xVars))

##
## Evaluate mixture of D0 and D1.
## Evaluate number of clusters
##
range.alpha <- seq(0, 1, by=0.05)
K <- 11
cr <- choicealpha(featDist, geoDist, range.alpha, K, graph=TRUE)

## 
## Perform spatial cluster analysis
##
tree <- hclustgeo(featDist, geoDist, alpha=0.2)
plot(tree, hang=-1)
rect.hclust(tree, k=K)

##
## Map Results
##
neighClus <- as.factor(cutree(tree, K))
table(neighClus)
mapColorQual(neighClus, tractShp, 
             map.title ="Spatially Constrainted Cluster Analysis",
             legend.title="Cluster\nNo.")
plot(lakesShp, col="skyblue", border="skyblue",add=T)
plot(hwyShp, col="cornsilk2", lwd=4, add=T)

##
## Means of variables by Cluster
##
clustMeans <- clusterStats(xVars, neighClus, func=mean)
layoutBoxPlots(xVars, neighClus, ncol=3, zTrans=T, varwidth=F)


library(maptools); library(spdep); library(DallasTracts); library(ClustGeo)

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

data(tractShp)
validTractShp <- tractShp[!is.na(tractShp$BUYPOW), ]        # Remove 2 airport tracts with NA's

##
## Generate graph and spherical distance matrices
##
nb <- spdep::poly2nb(validTractShp, queen=F)                 # extract first order neighbors links
B <- spdep::nb2mat(nb, style="B")                            # convert neighbor list to binary matrix
stepDist <- AdjOrder(B)                                      # distance matrix of tracts i-steps apart
sphDist <- sp::spDists(validTractShp, longlat=T)             # spherical distance matrix among tracts in km


### find the numeric variable
xVar <- as.data.frame(validTractShp@data)
new_xVar<- xVar[,sapply(xVar[,1:ncol(xVar)],function(x){is.numeric(x)})]

### Principal component analysis
### get rid of ID and missing value line
missList<- colnames(new_xVar)[sapply(new_xVar[,colnames(new_xVar)],function(x) sum(complete.cases(x))< nrow(new_xVar))]
new_xVar <- scale(new_xVar[,-1])
index <- c(1:ncol(new_xVar))[colnames(new_xVar) %in% missList]
pc <-prcomp(new_xVar[,-index], retx=TRUE)
### set a proper number of component
# plot(pc$sdev^2, type="b", main="Scree Plot", 
#      xlab="Component Order", ylab="Eigenvalue")
# abline(h=1, v=c(3,8), lty=5,lwd=2, col="red")
nofComp <- 8
# cumVar <- cumsum(pc$sdev^2)/length(pc$sdev)*100
# plot(cumVar, main="Percent Explained Variance",
#      type="b", xlab="Component Order", ylab="Relative Cummulative Variance")
# abline(v=nofComp, h=cumVar[nofComp], lty=5)

### use K-nearest neighborhood to fill missing value and do Principal component analysis again
library(DMwR)
pc$x
pc <- cbind(pc$x[,1:8],new_xVar[,index])
knnOutput <- knnImputation(pc)
pc1 <-prcomp(knnOutput, retx=TRUE)
# plot(pc1$sdev^2, type="b", main="Scree Plot",
#      xlab="Component Order", ylab="Eigenvalue")
# abline(h=1, v=c(3,8), lty=5,lwd=2, col="red")
nofComp <- 8

geoDist <- 1-B
diag(geoDist) <- 0
geoDist <- as.dist(geoDist)
featureDist <- dist(pc1$x[,1:nofComp])

cr <- choicealpha(featureDist, geoDist, range.alpha=seq(0, 1, 0.1), K=12, graph=TRUE)

tree <- hclustgeo(featureDist, geoDist, alpha=0.3)
P12ter <- cutree(tree, 12)
validTractShp$Type <- as.factor(P12ter)

mapColorQual(validTractShp$Type, validTractShp,
             map.title="Auto Division",
             legend.title = "Regions") 

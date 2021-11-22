library(DallasTracts); library(ClustGeo); library(DMwR); library(class); library(gmodels)
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
validTractShp <- tractShp[!is.na(tractShp$BUYPOW), ]         # Remove 2 airport tracts with NA's

##
## Generate graph and spherical distance matrices
##
nb <- spdep::poly2nb(validTractShp, queen=F)                 # extract first order neighbors links
B <- spdep::nb2mat(nb, style="B")                            # convert neighbor list to binary matrix
stepDist <- AdjOrder(B)                                      # distance matrix of tracts i-steps apart
max(stepDist)
sphDist <- sp::spDists(validTractShp, longlat=T)             # spherical distance matrix among tracts in km

##
## Calculate spatial distance matrices
##
geoDist <- as.dist(stepDist)
geoDist <- as.dist(sphDist)
geoDist <- 1-B; diag(geoDist) <- 0; geoDist <- as.dist(geoDist)

##
## Visulize first order neighbors
##
# plot(validTractShp, col="palegreen3", border=grey(0.9), axes=T) 
# plot(nb, coords=coordinates(validTractShp), pch=19, cex=0.1, col="blue", add=T)
# title("Spatial Neighbors Links among Tracts") 

##
## Select Variables for Dissimilarity. Include MEDVALHOME with NA's
##
varKeep <- c("POPDEN","HHMEDINC","PCTDAYPOP","MEDAGE","PCTWHITE","PCTBLACK","PCTHISPAN",
             "PCTUNIVDEG","PCTUNEMP","PCTNOHINS","PCTB2010","PCTB1990","PCTB1970",
             "PCTB1950","PCTBPRE","MEDVALHOME")

xVars <- as.data.frame(validTractShp)
xVars <- xVars[varKeep]
row.names(xVars) <- validTractShp$SeqId


##
## Use kNN to substitute NA's with with kNN regression estimates
##
summary(xVars)
xVars <- DMwR::knnImputation(xVars, k=5, scale=T, meth="weighAve")
summary(xVars)

##
## Calculate feature distance matrix
##
featDist <- dist(scale(xVars))

##
## Identify urban zones using cross-validatory classification
##
xVars <- scale(xVars)
zones <- tractShp@data[!is.na(tractShp$BUYPOW), "CITYPERI" ]

pred.zones <- knn.cv(xVars, zones, k=3, prob=TRUE)
CrossTable(zones, pred.zones,
           prop.chisq = FALSE, prop.t = FALSE, prop.c = FALSE, prop.r = TRUE,
           dnn = c('actual','predicted'))



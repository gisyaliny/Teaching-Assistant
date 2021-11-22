################################################################################
##                                                                            ##
## Cluster Analysis of European Protein Consumption                           ##
##                                                                            ##
################################################################################
rm(list=ls())
library(DallasTracts); library(gmodels)
setwd("E:\\Lectures2019\\GISC6323\\Lecture04\\OldEurope")

## Import Shape files
euro1980 <- rgdal::readOGR (dsn= ".", layer = "Euro1980Countries", integer64="allow.loss")
euroProt <- rgdal::readOGR (dsn= ".", layer = "EuroProteinMap", integer64="allow.loss")
centroids <- coordinates(euroProt)            # Get centroids of the European countries

plot(euro1980, bg="lightblue", axes=T, border="white", col=grey(0.9), xlim=c(-20,40), ylim=c(30,70))
mapColorQual(euroProt$Region, euroProt,
             map.title="Regions of Europe",
             legend.title = "Region",add.to.map=T)
box()
Protein <- data.frame(euroProt)

## Describe Types According to US Classification
by(Protein[ ,7:15], Protein$Region, colMeans)

## Prepare Data for Cluster Analysis
row.names(Protein) <- Protein$Country               # Assigning the row-names based on Country name
zProt <- scale(Protein[ ,7:15])                     # Scaling the metric variable

## Perform Clustering 
pDist <- dist(zProt,method="euclidian")             # Euclidean distance
pClus <- hclust(pDist,method="ward.D2")             # Clustering based on Ward method with squared distances

## Inspection Results
plot(pClus,xlab="Country",ylab="Dissimilaritiy",hang=-1)          # Plot Dendogram
myClass <- rect.hclust(pClus, k=6)                                # break dendrogram into clusters
plot(rev(c(0,pClus$height)), type="b",                            # Scree Plot
     ylim=c(0, max(pClus$height)),
     main="Determine # of Clusters",
     xlab="# of Clusters", ylab= "Between Clusters Heterogeneity")
abline(h=0, v=1, lty=2)     
## Select Number of Clusters
nClust <- 6                                                       # Number of clusters
abline(v=nClust,lty=2, lwd=2)                                     # Show break-line in scree plot
myClass <- cutree(pClus,k=nClust)                                 # Getting cluster IDs (factor)  

## Describing Clusters
cProt <-data.frame(Protein,myClass)                               # Add cluster labels to original dataframe

by(cProt[ ,7:15], cProt$myClass, colMeans)
cProt$myClass <- factor(cProt$myClass,                    # Assigning tentative cluster names
                         labels=c("myWestEurope","myMediterra","myEastEuro","myBalkan","myScania","MyIberia"))

## Compare identified clusters with given regionalization
CrossTable(x = cProt$Region, y = cProt$myClass, prop.chisq = FALSE)

## Plot identified food regions
plot(euro1980, bg="lightblue", axes=T, border="white", col=grey(0.9), xlim=c(-20,40), ylim=c(30,70))
mapColorQual(cProt$myClass, euroProt,
             map.title="Regions of Europe",
             legend.title = "My Region",add.to.map=T)
box()

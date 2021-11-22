##
## Objectives: Loads list "ipnew" of more than 1,413  package and installs these packages
## Note: Depending on internet connection the installation may take _several_ hours
##
 
## load dataframe from URL
load(url("http://thinkr.spatialfiltering.com/Downloads/Packages.txt"))  
ipold <- installed.packages()[,1]                       # get names of existing packages

##
## install packages not present in current installation
##

## Send all packages to system directory (currently commented out)
#.libPaths("C:/Program Files/Microsoft/R Open/R-3.5.3/library")  

for (i in setdiff(ipnew,ipold)){
  install.packages(i, dependencies=TRUE)
}

## Install private course package
install.packages("http://thinkr.spatialfiltering.com/Downloads/DallasTracts_0.2.2.tar.gz", repos=NULL)

cat("# of installed packages: ", length(installed.packages()[,1]))

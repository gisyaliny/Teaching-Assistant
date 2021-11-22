##
## Objectives: Loads list "ipnew" of more than 350  package and installs these packages
## Note: Depending on internet connection the installation may take _several_ hours
##
rm(list=ls())                                           # start with clean environment 

## load dataframe from URL
load(url("http://www.spatialfiltering.com//ThinkR/Downloads/Packages.txt"))  
ipold <- installed.packages()[,1]                       # get names of existing packages

##
## install packages not present in current installation
##

## Send all packages to system directory (currently commented out)
#.libPaths("C:\\Program Files\\R\\R-4.0.2\\library")  

for (i in setdiff(ipnew,ipold)){
  install.packages(i, dependencies=FALSE)
}

cat("# of installed packages: ", length(installed.packages()[,1]))

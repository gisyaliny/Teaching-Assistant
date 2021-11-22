rm(list=ls(all=TRUE))
library(ellipse); library(corrplot); library(car); library(RColorBrewer); library(rgl); library(ggplot2)
data(diamonds, package="ggplot2")

##
## Evaluation of a data set with 53940 observations
##

## Select subset of variables
diaSub <- subset(diamonds, select = c(cut,price,carat,depth,table,x,y,z))
summary(diaSub)

## Correlation matrix
corMat <- cor(diaSub[ ,-1], method= "pearson", use = "complete.obs")  # exclude nominal "cut"
round(corMat,2)                                                       # print rounded to 2 digits

## Define Colors using RColorBrewer
colors <- RColorBrewer::brewer.pal(11,"RdBu")   # Use RColorBrewer diverging red to blue palette
colors <- rev(colors)                           # reverse order of colors so negative correlations are blue

## Recode corMat so it matches the
colIdx <- round(5*corMat + 6, 0)                # cor=-1 -> 1; cor=0 -> 6; cor=1 -> 11

## Use library(ellipse) scatterplot function
ellipse::plotcorr(corMat, col=colors[colIdx])   # ellipse function

## Use library(corrplot) scatterplot function
corrplot::corrplot(corMat)                      # wrong default color coding !!!
corrplot::corrplot(corMat, col=colors, method="ellipse", order="FPC",
                   diag=FALSE, addCoef.col="green")

## see also Kabacoff section 11.1 for plots with many data points
plot(diaSub$price, diaSub$x, pch=".", main="Messy Data Overload")
smoothScatter(diaSub$price, diaSub$x, main="Density Smoother for a large # of observations")
## explore the # of bins
hist(diaSub$price, nclass=50)                   
hist(diaSub$x, nclass=50)
## Use hexbin functions 
bin <- hexbin::hexbin(diaSub$price, diaSub$x, xbins = 50)
hexbin::plot(bin, main="Hexagonal Binning of the Data Cloud")
## Now with ggplot
library(ggplot2); library(hexbin)
diaP <- ggplot(diaSub, aes(x=price, y=x))
diaP + stat_binhex(bins=50) + 
       scale_fill_gradient(low="lightblue", high="red", limits=c(0,3500))

##
## Evaluation of a data set with a reduced # of observations
##

## use a sample of 1000 observations to minimize computational burden and gain visual resolution
diaSample <- diaSub[sample(1:nrow(diaSub),1000), ]

## Bivariate plot with car::scatterplotMatrix
car::scatterplot(x~price, data=diaSample)
## Multivariate plot with car::scatterplotMatrix
car::scatterplotMatrix(~price+carat+depth+table, data=diaSample, diagonal="density") # is a CAR function. See formula syntax

## standard R scatterplot with interactive legend placement
plot(diaSample$carat,diaSample$price,pch=19,col=diaSample$cut,                       # color points according to retired status
     xlab="Carat",ylab="Price",main="Diamonds Data")
abline(v=mean(diaSample$carat),h=mean(diaSample$price),lty=2)                        # reference lines
abline(lm(price~carat, data=diaSample), col="red", lwd=2)                            # add regression line
## interactivel legend. Use mouse pointer to place at desired location
legend(locator(1),
       title="Cut:",legend=c("fair","good","very good","premium","ideal"),
       col=c(1,2,3,4,5),pch=19)

## Similar with ggplot
ggplot(diaSample, aes(x=carat, y=price)) +
  geom_point( aes(color=cut) ) +                                                      # only points are colored by variable "cut"
  geom_smooth(method="lm") +
  geom_hline(yintercept = mean(diaSample$price), linetype="dashed") +
  geom_vline(xintercept = mean(diaSample$carat), linetype="dashed") +
  labs(title="Relationship between Price and Carat", x="Carat", y="Price")

## Similar but augmented with car::scatterplot
car::scatterplot(price~carat|cut, data=diaSample, legend.coords="topleft",           # Enhanced car function. See "|" syntax
                 legend.title="Cut:", boxplots="xy", smooth=F, by.groups=FALSE)

## interactive 3D scatterplot. Look for RGL icon in task bar
rgl::plot3d(diaSample$carat, diaSample$price, diaSample$depth, col="red",
            xlab="Carat", ylab="Price", zlab="Depth",
            type="s",size=0.5)

## automatically spin rgl::plot3d plot
rgl::play3d(spin3d())

rm(list=ls())                                                                # clean workspace
crime <- read.csv("http://datasets.flowingdata.com/crimeRatesByState2005.tsv", header=TRUE, sep="\t")
head(crime)

## Test bubble plots
symbols(crime$murder, crime$burglary, circles=sqrt(crime$population/pi),     # Correct relative scaling by circle radius
        inches=0.6, main="Correct Circle Size")                              # Inch is just a global scaling parameter
symbols(crime$murder, crime$burglary, circles=crime$population, bg="yellow", # Incorrect relative scaling by circle area
        inches=0.6, main="Incorrect Circle Size - Overlapping Circles")

## Beautify plot
crime <- crime[order(crime$population, decreasing=TRUE), ]                   # Make sure that larger circles are drawn first
                                                                             # by decreasing sorting the df according to population
plot(burglary~murder, data=crime, cex=0.005*sqrt(crime$population/pi),       
     pch=21, bg="red", xlab="Murder Rate", ylab="Burglary Rate")
title(main="Comparison of Crime and Burglar Rates (per 100,000) in 2005")
text(crime$murder,crime$burglary,crime$state,cex=0.5)                       # add state names

abline( v=mean(crime$murder), lty=3)                                        # add "dotted" mean murder line
abline( h=mean(crime$burglary), lty=3)                                      # add "dotted" mean burglary line

crime.lm <- lm(crime$burglary~crime$murder)                                 # calibrate regression model
abline( crime.lm, lty=5, lwd=2)                                             # add "longdash" regression line


## 
## As ggplot graph
##

library(ggplot2)
## building graph in layers
g <- ggplot(data=crime, aes(x=murder, y=burglary, size=population))      # define data and coordinate system
g <- g + geom_point(shape=21,colour="black", fill="cornsilk")            # shape 21 has circumfence and fill area
g <- g + scale_size_area(max_size=30, guide=FALSE)                       # use area size instead of radius
g <- g + geom_text(aes(label=state), size=3, vjust=0)                    # add state labels
g <- g + stat_smooth(method="lm")                                        # add fancy regression line
g <- g + geom_hline(yintercept=mean(crime$burglary), linetype="dashed")  # add y mean line
g <- g + geom_vline(xintercept= mean(crime$murder), linetype="dashed")   # add x mean line
g <- g + labs(x="Murder Rate", y="Burglary Rate")
g <- g + ggtitle("Comparison of Crime and Burglar Rates\nper 100,000 Inhabitants in 2005") + 
         theme(plot.title=element_text(size=20, vjust = 2))
## finally plot the layers of the graph
g   

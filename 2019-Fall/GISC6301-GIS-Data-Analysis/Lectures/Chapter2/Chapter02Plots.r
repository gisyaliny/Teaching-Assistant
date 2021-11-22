##
## Book examples. Run one line at a time
##
library(ggplot2)
## Reading a workspace *.RData. Several data-sets are stored in this workspace.
load("E:\\Lectures2019\\GISC6301\\Chapter02\\BBRChapt02Data.RData")

## Figures 2-1 - Resort categories by frequency
lakeveg$Type <- lakeveg[ ,"Vegetation Type"]  # Rename for easier handling
table(lakeveg$Type) 
barplot(table(lakeveg$Type))
lakeveg$Type <- reorder(lakeveg$Type, rep(-1,length(lakeveg$Type)), "sum")
table(lakeveg$Type)
barplot(table(lakeveg$Type))

## GGPlot Bars
ggplot(lakeveg, aes(x=Type, fill=..count..)) + geom_bar(color="black") + labs(title="Vegetation Types")
## Pie-Chart
ggplot(lakeveg, aes(x=factor(1), fill=Type)) + geom_bar(width=1, color="black") + 
  coord_polar(theta="y") + labs(title="Vegetation Types")


## Figure 2-3
dodata$Value <- dodata[ ,"DO Value"]
( valueTab <- table(dodata$Value) )                # Why no sort here?
barplot(valueTab)                                  # Barplot wrong for measurement level

## Table 2-6
valueClass <- cut(dodata$Value,breaks=seq(3.99,7,by=0.5))   # Classify values into consecutative groups
table(valueClass)                                           # Count obs in groups

## Histogram Figure 2-4. Note that the hist function also returns a list of relevant statistics
( hist.table <- hist(dodata$Value,
                     breaks=seq(3.99,7,by=0.5),
                     main="",xlab="DO values, mg/L") )

## Histogram Fibure 2.4 with ggplot
ggplot(data=dodata, aes(x=Value, fill=..count..)) + 
  geom_histogram(binwidth=0.5, boundary=3.99) +
  labs(title="Desolved Oxygen", x="DO values, mg/L")

## Cumulative Frequency Histogram Figure 2-13 with ogive
( hist.table$counts <- cumsum(hist.table$counts) )                # vector with cummulative sums
plot(hist.table,main="",xlab="DO values, mg/L")                   # plot cummulative bars
lines(hist.table$breaks, c(0,hist.table$counts),lwd=2,lty=2)      # add ogive line. Include 0 as starting value

## Empirical Distribution Function
summary(ecdf(dodata[ ,"DO Value"]))
plot(ecdf(dodata[ ,"DO Value"]))

## ECDF with ggplot
ggplot(data=dodata, aes(x=Value)) + stat_ecdf() + 
  geom_hline(yintercept=c(0,1), linetype=5) +
  labs(title="Empirical Distribution Function of Desolved Oxygen", x="DO values, mg/L", y="Probability")

## Histograms Figure 2-12
hist(income$Income, breaks="Sturges", xlab="Median household income ($)", main="Sturges Rule")
hist(income[ ,"Income"], breaks=seq(29000,56000,length.out=9+1),
     xlab="Median household income ($)", main="9 classes")
hist(income[ ,"Income"], nclass=27, xlab="Median household income ($)", main="27 classes")

## Standard ggplot histogram with counts but rugs
ggplot(income, aes(x=Income)) + 
  geom_histogram(binwidth=2500, boundary=30000, fill="cornsilk", color="black") +
  geom_rug(sides="b", color="red") +
  labs(title="Income Distribution for U.S. States in 1999", x="Income ($) ") +
  xlim(25000,60000)

## ggplot histogram witt kernel density layer
ggplot(income, aes(x=Income, y=..density..)) + 
  geom_histogram(binwidth=2500, boundary=30000, fill="cornsilk", color="black")+   # experiment with bins, binwidth and boundary
  geom_density(col="red", adjust=0.5) +                           # experiment with smoothing parameter adjust
  labs(title="Income Distribution for U.S. States in 1999", x="Income ($) ") +
  xlim(25000,60000)


##
## Boxplots with MyPower Dataset
##

## One variable grouped by one factor
boxplot(kWhBill~Month, data=MyPower,                                    # Syntax: metricVar ~ factorVar
        notch=TRUE, varwidth=TRUE, ylab="kWh Consumed", xlab="Month")   # Are the options necessary and too fancy?

## Same as beautiful ggplot
ggplot(MyPower, aes(x=Month, y=kWhBill)) +
  geom_boxplot(fill="cornsilk", color="black", notch=FALSE) +
  geom_point(color="red", alpha=0.5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="red") +
  geom_rug(sides="l", color="black") +
  labs(title="Average KWh Consumption by Month", x="Month", y="Consumption in kWh")


## convert data from wide to long format. Approach needed in ggplot
library(reshape2)                                                       # Has function "melt" allowing to restructure a dataframe

PowerLong <- melt(MyPower, id.vars= "SeqID",                            # Stack temperature into one column. Use "" here.
                  measure.vars = c("MinTemp","AveTemp","MaxTemp"),
                  variable.name= "Variability", value.name= "Temperature")

boxplot(Temperature~Variability, data=PowerLong)                        # Standard boxplot

## Same in ggplot
ggplot(PowerLong, aes(x=Variability, y=Temperature)) +
  geom_boxplot(fill="cornsilk", color="black") +
  geom_point(color="red", position="jitter", alpha=0.5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=5, fill="red") +
  geom_rug(sides="l", color="red")

## for standard boxplots there is an easier way
boxplot(MyPower[ , c("MinTemp","AveTemp","MaxTemp")])                   # Select several variable in data set
                                                                        # Syntax: DataFrame[c("var1","var2",...)]


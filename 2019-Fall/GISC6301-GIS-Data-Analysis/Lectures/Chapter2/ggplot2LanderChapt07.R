##
## Example based on Lander, Chapter 7: Statistical Graphics
##
library(ggplot2)
data(diamonds)
head(diamonds)

hist(diamonds$carat, main="Carat Histogram", xlab = "Carat")
plot(price ~ carat, data=diamonds)
plot(diamonds$carat, diamonds$price, col=diamonds$color)

boxplot(diamonds$carat)
boxplot(diamonds$carat~diamonds$cut, ylab="Carat", xlab="Cut")

## Now with ggplot2
ggplot(data=diamonds) + geom_histogram(aes(x=carat))
ggplot(data=diamonds) + geom_histogram(aes(x=carat), binwidth = 0.1)
ggplot(data=diamonds, aes(x=carat)) + geom_histogram()
ggplot(data=diamonds) + geom_density(aes(x=carat), fill="grey50")

ggplot(data=diamonds) + aes(x=carat, y=price) + geom_point()
g <- ggplot(data = diamonds, aes(x=carat, y=price))
g + geom_point(aes(color=color))

g + geom_point(aes(color=color)) + facet_wrap(~color)
g + geom_point(aes(color=color)) + facet_grid(cut ~ clarity)

ggplot(data = diamonds, aes(y=carat, x=1)) + geom_boxplot()
ggplot(data = diamonds, aes(y=carat, x=cut)) + geom_boxplot()

ggplot(data = diamonds, aes(y=carat, x=cut)) + geom_violin()
ggplot(data = diamonds, aes(y=carat, x=cut)) + geom_violin() + geom_point()
ggplot(data = diamonds, aes(y=carat, x=cut)) + geom_point() + geom_violin() 

## Sort factor color by frequency
data("diamonds")
( colorTab <- table(diamonds$color) )
diamonds$color <- reorder(diamonds$color, rep(-1,length(diamonds$color)), "sum")
( colorTab <- table(diamonds$color) )
ggplot(diamonds, aes(x=color, fill=..count..)) + geom_bar(color="black") 

## Line charts
data(economics)
head(economics)

ggplot(data = economics, aes(x=date, y=pop)) + geom_line(aes(group=1))

library(lubridate)    # may need to be installed
library(scales)
economics$year <- year(economics$date)
economics$month <- month(economics$date, label=TRUE)
econ2000 <- subset(economics, economics$year >= 2000)

ge <- ggplot(data = econ2000, aes(x=month, y=pop))
ge <- ge + geom_line(aes(color=factor(year), group=year))
ge <- ge + scale_color_discrete(name="Year")
ge <- ge + scale_y_continuous(labels=comma)
ge <- ge + labs(title="Population Growth", x="Month", y="Population")
ge

## Themes
library(ggthemes)    # may need to be installed

gt <- ggplot(data=diamonds, aes(y=price, x=carat)) + geom_point(aes(color=color))
gt + theme_economist() + scale_color_economist()
gt + theme_excel() + scale_color_excel()
gt + theme_tufte()
gt + theme_wsj()

##
## Examples from Kabacoff (2015). R in Action. Data analysis and graphics in R.
## Chapter 19: Advanced graphics with ggplot2
##

library(ggplot2)
data(mtcars, package="datasets")
help(mtcars)
head(mtcars)

mtcars$am <- factor(mtcars$am, levels=c(0,1), labels=c("automatic", "manual"))
mtcars$vs <- factor(mtcars$vs, levels=c(0,1), labels=c("V arrangement", "Linear arrangement"))
mtcars$cyl <- factor(mtcars$cyl)
sapply(mtcars,is.factor)

## Combination of layers
ggplot(data=mtcars, aes(x=wt, y=mpg)) +
  geom_point(pch=17, col="blue", size=2) +
  geom_smooth(method="lm", color="red", linetype=2) +
  labs(title="Automobile Data", x="Weight", y="Miles per Gallon")
 
## Facet of plots
ggplot(data=mtcars, aes(x=wt, y=mpg, color=cyl, shape=cyl)) +
  geom_point(size=3) +
  facet_grid(am~vs) +
  labs(title="Automobile Data", x="Weight", y="Miles per Gallon")


## Geom functions Section 19.3
data(singer, package="lattice")
head(singer)
summary(singer)

## Histograms
ggplot(singer, aes(x=height)) + geom_histogram()
ggplot(singer, aes(x=height)) + geom_histogram(binwidth = 1)

ggplot(singer, aes(x=height, y=..density..)) + geom_histogram(bins = 10) +
  geom_density(col="red", adjust=0.5) + xlim(55,80)

## Side-by-side box plots
ggplot(singer, aes(x=voice.part, y=height)) + geom_boxplot()
ggplot(singer, aes(x=voice.part, y=height)) + 
  geom_violin(fill="lightblue") + 
  geom_boxplot(fill="lightgreen", width=0.2)

data(Salaries, package="carData")
head(Salaries)

ggplot(Salaries, aes(x=rank, y=salary)) +
  geom_boxplot(fill="cornflowerblue", color="black", notch=TRUE) +
  geom_point(position="jitter", color="blue", alpha=0.5) +
  geom_rug(sides="l", color="black") +
  labs(title="Nine Month Academic Income by Rank (2008-2009)", x="Rank", y="Salary in $")

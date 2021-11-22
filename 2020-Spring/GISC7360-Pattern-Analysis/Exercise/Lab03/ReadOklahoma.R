library(spatstat)
data("okblack", package = "splancs")
data("okwhite", package = "splancs")

ppblack <- data.frame(x=okblack$x,y=okblack$y,mark="black")
ppwhite <- data.frame(x=okwhite$x,y=okwhite$y,mark="white")
ppboth <- rbind(ppblack,ppwhite)
range(ppboth$x);range(ppboth$y)

ppOffense <- with(ppboth, ppp(x,y,c(100,400),c(50,350), marks=mark))
summary(ppOffense)
plot(ppOffense);axis(1);axis(2)

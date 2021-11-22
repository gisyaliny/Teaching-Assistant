n <- 500
p.val <- 0.5                     # fair coin or not ?
x <- rbinom(n, 1, p.val)         # n binary 0 or 1 random variable
x.cumsum <- cumsum(x)            # cumulative sum of ones after each repeated experiment
seq.id <- 1:n
top.val <- seq.id[x > p.val]     # identify ones in the stream of n experiments
bot.val <- seq.id[x < p.val]     # identify zeros in the stream of n experiments

plot(x.cumsum/seq.id, type="l",col="red",lwd=2,ylim=c(0,1),
     xlab="Cummulative Number of Experiments",
     ylab=expression(Pr(Head)%~~% "#Heads"/"#Trials"))
text(-5,0,"T")
text(-5,1,"H")
title(main="Repeated Tossing of a Coin")
rug(top.val,side=3)                                 # plot individual outcomes
rug(bot.val,side=1)

abline(h=p.val,lty=5)
lines(seq.id,p.val+2*sqrt(p.val^2/seq.id),lty=3)    # upper confidence limit
lines(seq.id,p.val-2*sqrt(p.val^2/seq.id),lty=3)    # upper confidence limit
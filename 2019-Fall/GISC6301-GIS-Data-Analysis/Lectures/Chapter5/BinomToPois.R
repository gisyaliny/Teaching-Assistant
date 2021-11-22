rm(list=ls(all=TRUE))            # start clean
##
## Asymptotic Approach of Binomial distribution to Poisson distribution
## See BBR pp 228-230
##
x <- 1                           # x must be less equal than min(nSize). Why?
n <- 4
prob <- 0.25
nSize <- n^seq(0, 16, by=1)      # adjust starting value for different x
probSuccess <- prob/nSize
binomProb <- dbinom(x=x, size=nSize, prob= probSuccess)

round(data.frame("n"=nSize,"ProbSuccess"=probSuccess,"ProbXeq1"=binomProb),6)

dpois(x=x, lambda=prob)          # does dbinom approach dpois?

data(Titanic)
class(Titanic)
Titanic <- as.data.frame(Titanic)
class(Titanic)
names(Titanic)
tab <- xtabs(Freq~Survived+Sex, data=Titanic)
class(tab)
tab

TitanicIndividual <- as.data.frame(lapply(Titanic, function(x) rep(x, Titanic$Freq)))
table(TitanicIndividual$Survived,TitanicIndividual$Sex)

(TitanicMat <- matrix(c(1364,367,126,344), nrow=2, ncol=2))

( chi1 <- chisq.test(tab) )
( chi2 <- chisq.test(TitanicIndividual$Survived,TitanicIndividual$Sex) )

chi1$observed
chi1$expected
chi1$residuals

rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console


library(pROC); library(caret)

trueClass <- as.factor(c(rep("negative",10),rep("positive",10)))
pCutOff <- seq(0.1, 0.9, by=0.1)

## Recall:
##   Sensitivity: Prob predict positive given true positive
##   Specificity: Prob predict negative given true negative

## Task 1 a
table1Probs <- c(seq(0.00, 0.45, by=0.05), seq(0.55, 1.00, by=0.05))

sensTab1 <- NULL
specTab1 <- NULL

for(i in pCutOff){
  specTab1 <- c(specTab1, sum(table1Probs[1:10] < i)/10)
  sensTab1 <- c(sensTab1, sum(table1Probs[11:20] >= i)/10)
  
  preClass <- as.factor(ifelse(table1Probs < i, "negative","positive"))
  print(table(trueClass, preClass))
}
specTab1 # True Negative
sensTab1 # True Positive

rocTable1 <- roc(trueClass, table1Probs)
plot(rocTable1, main="ROC: Curve for Table 1", 
     col="blue", lwd=2, legacy.axes=FALSE)
auc(rocTable1)

## Task 1 b
table2Probs <- c(0.55,0.05,0.65,0.15,0.75,0.25,0.85,0.35,0.95,0.45,
                 0.00,0.60,0.10,0.70,0.20,0.80,0.30,0.90,0.40,1.00)

sensTab2 <- NULL
specTab2 <- NULL

for(i in pCutOff){
  specTab2 <- c(specTab2, sum(table2Probs[1:10] < i)/10)
  sensTab2 <- c(sensTab2, sum(table2Probs[11:20] >= i)/10)
  
  preClass <- as.factor(ifelse(table2Probs < i, "negative","positive"))
  print(table(trueClass, preClass))
}
specTab2 # True Negative
sensTab2 # True Positive

rocTable2 <- roc(trueClass, table2Probs)
plot(rocTable2, main="ROC: Curve for Table 2", 
     col="blue", lwd=2, legacy.axes=FALSE)
auc(rocTable2)

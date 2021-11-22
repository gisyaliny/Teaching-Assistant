##
## Functions for ROC curves
##
library(ROCR)
rocplot <- function(pred, truth, ...){
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf,...)
} ##end::rocplot

##
## Basic building block
##
idx <- order(-predProbCase)
# control == 0 and case == 1
recall <- cumsum(contcase[idx]==1)/sum(contcase==1)
specificity <- (sum(contcase==0)- cumsum(contcase[idx]==0)) / sum(contcase==0)
rocDf <- data.frame(recall=recall, specificity=specificity)

ggplot(rocDf, aes(x=specificity, y=recall)) +
  geom_line(color="blue") +
  scale_x_reverse(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  geom_line(data=data.frame(x=(1:100)/100), aes=(x=x,y=1-x),
            linetype="dotted", color="red")

auc <- sum(rocDf$recuall[-1]*diff(1-rocDf$specificity))
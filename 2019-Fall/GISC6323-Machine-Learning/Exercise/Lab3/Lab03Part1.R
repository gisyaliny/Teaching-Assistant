rm(list=ls())
library(ROCR);library(caret)
setwd("E:\\Lectures2019\\GISC6323\\Lecture07")

mCrossTable <- function(model,test_data){
  predicted <- factor(predict(model, test_data,type="class"))
  (cross_table <-  confusionMatrix(test_data$default, predicted,positive = "yes",
                                   dnn = c('Observed default', 'predicted default')))
}

aucPlot <- function(prediction,Observed){
  perf <- performance(prediction(prediction,Observed),"tpr","fpr")
  rocDf <- data.frame(x = perf@x.values[[1]],y = perf@y.values[[1]])
  auc <- sum(rocDf$y[-1]*diff(rocDf$x))
  ggplot(rocDf, aes(x= x, y= y)) +
    geom_line(color="blue") +
    scale_y_continuous(expand=c(0,0))+
    geom_line(data=data.frame(x=(1:100)/100, y=(1:100)/100), aes(x=x,y=y),
              linetype="dashed", color="red") +
    labs(title= paste("ROC curve with AUC:", round(auc, 5)),
         x = "False Positive Rate (1-Specificity)",
         y = "True Positive Rate (Sensitivity)")
}

#### set training and testing datasets ####

df <- read.csv("credit.csv")
df$default <- factor(df$default)
table(df$default)
idx <- sample(1:nrow(df),250)
train_df <- df[idx,]
test_df <-  df[-idx,]

####  Logestic  ####

library(MASS)
full.model <- glm(default ~., data = train_df, family = binomial)
summary(full.model)
step.model <- MASS::stepAIC(full.model)
summary(step.model)
ml_prediction <- predict(step.model, test_df, type="response")
log_class <- factor(ifelse(ml_prediction > 0.5,"yes","no"))
(cross_table <-  confusionMatrix(test_df$default, log_class,positive = "yes",
                                 dnn = c('Observed default', 'predicted default')))
aucPlot(ml_prediction,test_df$default)

####  Decision Tree & Pruned Tree  ####

library(tree);library(rpart.plot)
deci_tree <-  tree(default~., train_df)
mCrossTable(deci_tree,test_df)
aucPlot(predict(deci_tree, test_df)[,2],test_df$default)
plot(deci_tree);text(deci_tree, pretty=0)
prune.carseats <- prune.misclass(deci_tree, best=5)
plot(prune.carseats);text(prune.carseats, pretty=0)
mCrossTable(prune.carseats,test_df)
aucPlot(predict(prune.carseats, test_df)[,2],test_df$default)


#### Randomforest with bagging ####

library(randomForest);
bag_rf <- randomForest(default~., data=train_df,mtry=6, importance=TRUE, ntree=500)
importance(bag_rf)
mCrossTable(bag_rf,test_df)
aucPlot(predict(bag_rf, test_df,type = "prob")[,2],test_df$default)

#### C5 Boosting  ####

library(C50)
result <- NULL
for (i in 1:20){
  boost <- C5.0(default~. , data=train_df, trials = i)
  predicted <- factor(predict(boost, test_df,type="class"))
  result <- rbind(result, c(i, confusionMatrix(predicted, test_df$default)$overall[1])) # Get accuracy
}
result
plot(result, type="l")
boost5 <- C5.0(default~. , data=train_df, trials = 5)
mCrossTable(boost5,test_df)
aucPlot(predict(boost5, test_df,type = "prob")[,2],test_df$default)



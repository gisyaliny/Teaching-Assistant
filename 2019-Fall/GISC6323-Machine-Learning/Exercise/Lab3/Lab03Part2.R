rm(list=ls())
library(ROCR);library(caret)
RNGversion("3.5.3"); set.seed(12345)
setwd("E:\\Lectures2019\\GISC6323\\Lecture07")

df <- read.csv("redwines.csv")
df[,1:11] <- scale(df[,1:11])
idx <- sample(1:nrow(df),floor(nrow(df) * 0.2))
train_df <- df[idx,]
test_df <-  df[-idx,]


#### Lasso ####
library(glmnet)
x <- model.matrix(quality~., data = df)[,-1]
y <- df$quality

cv.out <- cv.glmnet(x[idx,], y[idx], alpha=1)
plot(cv.out)
cv.out$lambda.min

lasso.mod <- glmnet(x[idx,], y[idx], alpha=1)
coef <- predict(lasso.mod, type="coefficients", s=cv.out$lambda.min)[1:12]
coef[coef != 0]
plot(lasso.mod, xvar="lambda", label=TRUE); abline(h=0, v=log(cv.out$lambda.min), lwd=2)

x.test <- model.matrix(quality ~., test_df)[,-1]
lasso.pred <- predict(lasso.mod, s=cv.out$lambda.min, type="response", newx=x[-idx,])
Metrics::mse(test_df$quality, lasso.pred)

#### generalized additive model ####
library(gam)
(lasso.coef <- predict(lasso.mod, type="coefficients",s=cv.out$lambda.min))
var_lst <- c("volatile.acidity","chlorides","total.sulfur.dioxide","pH","sulphates","alcohol")
car::scatterplotMatrix(as.formula(paste("~quality", paste(var_lst, collapse=" + "), sep=" + ")),data = train_df)
gam1 <- gam(quality~s(volatile.acidity,4)+s(total.sulfur.dioxide,3)+pH + sulphates+ alcohol  + chlorides, data=train_df)
plot(gam1, se=TRUE, col="blue")
pred3 <- predict(gam1,test_df)
Metrics::mse(test_df$quality, pred3)

#### Pruned Tree ####
library(tree);library(rpart.plot)
npr_tree <- tree(quality~.,data=train_df)
plot(npr_tree); text(npr_tree, pretty=0)
npr_cvtree <- cv.tree(npr_tree)
plot(npr_cvtree$size, npr_cvtree$dev, type='b')
pr_tree <- prune.tree(npr_tree, best=5)
plot(pr_tree); text(pr_tree, pretty=0)
pred1 <- predict(pr_tree,test_df)
Metrics::mse(test_df$quality, pred1)

#### randomforest ####
set.seed(12345)
rf.wine <- randomForest(quality~.,data=train_df, mtry=6, importance=TRUE, ntree=100)
pred <- predict(rf.wine,test_df)
Metrics::mse(test_df$quality, pred)

####  Boosted Tree ####
set.seed(12345)
library(gbm)
boost_tree <- gbm(quality~.,data=train_df, distribution="gaussian", n.trees=5000, interaction.depth=4)
pred2 <- predict(boost_tree, newdata=test_df, n.trees=5000)
Metrics::mse(test_df$quality, pred2)

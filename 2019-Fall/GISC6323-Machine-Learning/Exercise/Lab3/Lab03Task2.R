rm(list=ls())
df <- read.csv("redwines.csv")
df[,1:11] <- scale(df[,1:11]) 
set.seed(1212)
indx <- sample(1:nrow(df),floor(nrow(df) * 0.2))
train_df <- df[indx,]
test_df <-  df[-indx,]


#### Lasso ####
library(glmnet)
x <- model.matrix(quality~., data = df)[,-1]
y <- df$quality
set.seed(1212)
cv.out <- cv.glmnet(x[indx,], y[indx], alpha=1)
x.test <- model.matrix(quality ~., test_df)[,-1]
lasso.mod <- glmnet(x[indx,], y[indx], alpha=cv.out$lambda.min)
lasso.pred <- predict(lasso.mod, s=cv.out$lambda.min, type="response", newx=x[-indx,])
Metrics::mse(test_df$quality, lasso.pred)

#### generalized additive model ####
library(gam)
(lasso.coef <- predict(lasso.mod, type="coefficients",s=cv.out$lambda.min))
var_lst <- c("volatile.acidity","chlorides","total.sulfur.dioxide","pH","sulphates","alcohol")
car::scatterplotMatrix(as.formula(paste("~quality", paste(var_lst, collapse=" + "), sep=" + ")),data = train_df)
gam1 <- lm(quality~ns(volatile.acidity,4)+ns(total.sulfur.dioxide,3)+pH + sulphates+ alcohol  + chlorides,data=train_df)
pred3 <- predict(gam1,test_df)
Metrics::mse(test_df$quality, pred3)

#### Pruned Tree ####
set.seed(1212)
npr_tree <- tree(quality~.,data=train_df)
npr_cvtree <- cv.tree(npr_tree)
plot(npr_cvtree$size,npr_cvtree$dev, type='b')
pr_tree <- prune.tree(npr_tree, best=5)
pred1 <- predict(pr_tree,test_df)
Metrics::mse(test_df$quality, pred1)

#### randomforest ####
set.seed(1212)
rf.wine <- randomForest(quality~.,data=train_df, mtry=6, importance=TRUE, ntree=100)
pred <- predict(rf.wine,test_df)
Metrics::mse(test_df$quality, pred)

####  Boosted Tree ####
set.seed(1212)
library(gbm)
boost_tree <- gbm(quality~.,data=train_df, distribution="gaussian", n.trees=5000, interaction.depth=4)
pred2 <- predict(boost_tree, newdata=test_df, n.trees=5000)
Metrics::mse(test_df$quality, pred2)

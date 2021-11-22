rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

## Data manipulation and visualization

library(rsample)   # splitting the data into training and test datasets

credit <-read.csv("credit.csv",header = TRUE, stringsAsFactors = TRUE)
sapply(credit, is.factor)

# Stratified sampling with the rsample package
set.seed(123)
split <- initial_split(credit, prop = 0.7, 
                       strata = "default")
default_train  <- training(split)
default_test   <- testing(split)

##
## Logistic Regression
##
creBase <- glm(default~1 , data=default_train, family=binomial)
summary(creBase)
creStep <- step(creBase, scope=~ checking_balance + months_loan_duration + credit_history + 
                  purpose + amount + savings_balance + employment_duration + 
                  percent_of_income + years_at_residence + age + other_credit + 
                  housing + existing_loans_count + job + dependents + phone,
                direction="forward")
summary(creStep)
predGLM <- predict(creStep, newdata=default_test, type="response")
plot(predGLM~default, data=default_test); abline(h=0.5, col="red")
plot(default~predGLM, data=default_test)


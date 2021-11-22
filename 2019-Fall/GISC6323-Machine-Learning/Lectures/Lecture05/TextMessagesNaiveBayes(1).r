##### Source: Brett Lantz, 2019. "Machine Learning with R. 3rd edition. Packt>
##### Chapter 4: Classification using Naive Bayes --------------------

## Example: Filtering spam SMS messages ----
## Step 2: Exploring and preparing the data ---- 

# read the sms data into the sms data frame
setwd("E:\\Lectures2019\\GISC6323\\Lecture05")
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)

# examine the structure of the sms data
str(sms_raw)

# convert spam/ham to factor.
sms_raw$type <- factor(sms_raw$type)

# examine the type variable more carefully
str(sms_raw$type)
table(sms_raw$type)

# build a corpus using the text mining (tm) package
library(tm)
sms_corpus <- VCorpus(VectorSource(sms_raw$text))

# examine the sms corpus
print(sms_corpus)
inspect(sms_corpus[1:2])

as.character(sms_corpus[[1]])
lapply(sms_corpus[1:2], as.character)

# clean up the corpus using tm_map()
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))

# show the difference between sms_corpus and corpus_clean
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])

sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers) # remove numbers
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords()) # remove stop words
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation) # remove punctuation

# tip: create a custom function to replace (rather than remove) punctuation
removePunctuation("hello...world")
replacePunctuation <- function(x) { gsub("[[:punct:]]+", " ", x) }
replacePunctuation("hello...world")

# illustration of word stemming
library(SnowballC)
wordStem(c("learn", "learned", "learning", "learns"))

sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace) # eliminate unneeded whitespace

# examine the final clean corpus
lapply(sms_corpus[1:3], as.character)
lapply(sms_corpus_clean[1:3], as.character)

# create a document-term sparse matrix
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

# alternative solution: create a document-term sparse matrix directly from the SMS corpus
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))

# alternative solution: using custom stop words function ensures identical result
sms_dtm3 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = function(x) { removeWords(x, stopwords()) },
  removePunctuation = TRUE,
  stemming = TRUE
))

# compare the result
sms_dtm
sms_dtm2
sms_dtm3

# creating training and test datasets
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]

# also save the labels
sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels  <- sms_raw[4170:5559, ]$type

# check that the proportion of spam is similar
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

# word cloud visualization
library(wordcloud)
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)

# subset the training data into spam and ham groups
spam <- subset(sms_raw, type == "spam")
ham  <- subset(sms_raw, type == "ham")

wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

sms_dtm_freq_train <- removeSparseTerms(sms_dtm_train, 0.999)
sms_dtm_freq_train

# indicator features for frequent words
findFreqTerms(sms_dtm_train, 5)

# save frequently-appearing terms to a character vector
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)

# create DTMs with only the frequent terms
sms_dtm_freq_train <- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

## Step 3: Training a model on the data ----
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_train_labels)

## Step 4: Evaluating model performance ----
sms_test_pred <- predict(sms_classifier, sms_test)

library(gmodels)
CrossTable(sms_test_labels, sms_test_pred, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual','predicted'))

## Step 5: Improving model performance ----
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_labels, sms_test_pred2, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual', 'predicted'))

##### Chapter 10: Evaluating Model Performance -------------------

## Create the predicted probabilities from the SMS classifier built in Chapter 4.
## NOTE: THIS SECTION WILL NOT RUN WITHOUT RUNNING THE CHAPTER 4 CODE TO CREATE THE SMS CLASSIFIER!

# obtain the predicted probabilities
sms_test_prob <- predict(sms_classifier2, sms_test, type = "raw")
head(sms_test_prob)

# combine the results into a data frame
sms_results <- data.frame(actual_type = sms_test_labels,
                          predict_type = sms_test_pred2,
                          prob_spam = round(sms_test_prob[ , 2], 5),
                          prob_ham = round(sms_test_prob[ , 1], 5))

# the first several test cases
head(sms_results)

# test cases where the model is less confident
head(subset(sms_results, prob_spam > 0.40 & prob_spam < 0.60))

# test cases where the model was wrong
head(subset(sms_results, actual_type != predict_type))

# specifying vectors
table(sms_results$actual_type, sms_results$predict_type)

# alternative solution using the formula interface (not shown in book)
xtabs(~ actual_type + predict_type, sms_results)

# using the CrossTable function
CrossTable(sms_results$actual_type, sms_results$predict_type,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual','predicted'))

# accuracy and error rate calculation --
# accuracy
(155 + 1202) / (155 + 1202 + 5 + 28)
# error rate
(5 + 28) / (155 + 1202 + 5 + 28)
# error rate = 1 - accuracy
1 - 0.976259

## Beyond accuracy: other performance measures ----
library(caret)
confusionMatrix(sms_results$predict_type, sms_results$actual_type, positive = "spam")

sensitivity(sms_results$predict_type, sms_results$actual_type, positive = "spam")
specificity(sms_results$predict_type, sms_results$actual_type, negative = "ham")
posPredValue(sms_results$predict_type, sms_results$actual_type, positive = "spam")

## Visualizing Performance Tradeoffs ----
library(pROC)
sms_roc <- roc(sms_results$actual_type, sms_results$prob_spam)

# ROC curve for Naive Bayes
plot(sms_roc, main = "ROC curve for SMS spam filter", col = "blue", lwd = 2, legacy.axes = TRUE)

##
## compare to kNN classifier results
##
sms_train_num <- ifelse(sms_train[,2:1139] == "No",0,1)
sms_test_num <- ifelse(sms_test[,2:1139] == "No", 0,1)
sms_test_pred_knn <- knn(sms_train_num, sms_test_num, sms_train_labels, k=9, prob=TRUE)
# combine the results into a data frame
probs <- attr(sms_test_pred_knn,"prob")
sms_knn_results <- data.frame(actual_type = sms_test_labels,
                              predict_type = sms_test_pred_knn,
                              prob_spam = ifelse(sms_test_pred_knn=="ham", 1-probs, probs),
                              prob_ham = ifelse(sms_test_pred_knn=="spam", 1-probs, probs))


#sms_results_knn <- read.csv("sms_results_knn.csv")
sms_roc_knn <- roc(sms_results$actual_type, sms_knn_results$prob_spam)
plot(sms_roc_knn, col = "red", lwd = 2, add = TRUE)

# calculate AUC for Naive Bayes and kNN
auc(sms_roc)
auc(sms_roc_knn)

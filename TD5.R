setwd("C:/Users/pc/OneDrive/Uni/R_AI Fundamentals/SNA/")


spam <- read.csv( "spam.csv", header=TRUE)

dim(spam)
names(spam)
summary(spam)

calc_acc = function(predicted, actual) {
  mean(predicted == actual)
}

library(MASS)
library(ggplot2)
library(caTools)
library(rpart)

set.seed(123)
split = sample.split(spam$spam, SplitRatio = 0.75)
spam_trn = subset(spam, split == TRUE)
spam_tst = subset(spam, split == FALSE)


dim(spam_trn)
dim(spam_tst)

typeof(spam)


head(spam_trn)

spam_tree <-rpart(spam ~ ., data=spam_trn)
printcp(spam_tree)


plot(spam_tree)
text(spam_tree, pretty = 0)
title(main = "Classificasion Tree")

model1 <- glm(spam~., family = "binomial", data = spam_trn)
model1

model2 <- gbm(spam~., data=spam_trn)
model2

calc_acc(spam_tree, spam_tst)
calc_acc(model1, spam_tst)
calc_acc(model2, spam_tst)

pred.glm = predict(classifier.logreg, newdata = test_set, type="response")
pred.glm_T_F = ifelse(pred.glm >= 0.5, TRUE,FALSE)
acc_logreg = calc_acc(pred.glm_T_F, test_set$spam)
spam_tree_pred = predict(spam_tree, test_set, type="class") 
acc_tree = calc_acc(spam_tree_pred, test_set$spam)
#bagging
spam_bag = randomForest(spam ~ ., data = training_set, mtry = 57, importance = TRUE, ntrees = 500)


library(rpart.plot)
rpart.plot(spam_tree)
library(PRP)
prp(spam_tree)

print(spam_tree)

summary(spam_tree)

rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}
◘
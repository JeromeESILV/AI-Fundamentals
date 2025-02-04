library(MASS)
library(ggplot2)
dim(Boston)
head(Boston)

set.seed(18)
boston_idx = sample(1:nrow(Boston), nrow(Boston) / 2)
boston_trn = Boston[boston_idx,]
boston_tst = Boston[-boston_idx,]

dim(boston_trn)
dim(boston_tst)

typeof(Boston)

library(rpart)

Boston_tree <-rpart(medv ~ ., data=boston_trn)
printcp(Boston_tree)


plot(Boston_tree)
text(Boston_tree, pretty = 0)
title(main = "Regression Tree")

library(rpart.plot)
rpart.plot(Boston_tree)
library(PRP)
prp(Boston_tree)

print(Boston_tree)

summary(Boston_tree)

rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}
◘
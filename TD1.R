# load MASS package
library(MASS)

# Check the dimensions of the Boston dataset
dim(Boston)

head(Boston)

train = 1:400
test = -train

# Speficy that we are going to use only two variables (lstat and medv)
#variables = which(names(Boston) ==c("lstat", "medv"))
training_data = Boston[train, variables]
testing_data = Boston[test, variables]

dim(training_data)#400
dim(testing_data)#106
head(testing_data)

plot(training_data)

lstatTrain = training_data$lstat
medvTrain = training_data$medv

lstatTest = testing_data$lstat
medvTest = testing_data$medv

plot(log(lstatTrain), training_data$medv)

#scatter.smooth(x=log(lstat), y=medv, main="Linear Regression")
model = lm(medv ~ log(lstat), data = training_data)
model

abline(model)

predict(model, data.frame(lstatTrain = c(5)))
predict(model, data.frame(lstatTrain = c(10)))
predict(model, data.frame(lstatTrain = c(15)))

prediction = predict(model, data.frame(lstat = lstatTest))
prediction
#plot(training_data$lstat, training_data$medv,xlab ="lstat", ylab="medv" )

library(datarium)
library(ggpubr)

data("marketing", package = "datarium")
head(marketing, 4)

summary(res.lm)

summary(marketing)

scatter.smooth(x=marketing$sales, y=marketing$youtube, xlab="Sales", ylab="Youtube AD Budget")

cor(x=marketing$sales, y=marketing$youtube)

res.lm <- lm(sales ~ youtube*facebook, data = marketing)
summary(res.lm)

model2 <- lm(sales ~ youtube, data = marketing)
model2

ggplot(marketing, aes(youtube, sales)) + geom_point() + stat_smooth(method = lm)

help(summary)
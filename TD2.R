# load MASS package
library(MASS)
library(corrplot)

# Check the dimensions of the Boston dataset
dim(Boston)

head(Boston)

train = 1:400
test = -train

variables = names(Boston)
training_data = Boston[train, variables]
testing_data = Boston[test, variables]

dim(training_data)#400
dim(testing_data)#106
head(testing_data)

cor(x=testing_data$age, y=testing_data$medv)

plot(x=training_data$medv, y=training_data$age)

model <- lm(medv ~ age, data = training_data)
model

#plot(x=testing_data$medv, y=testing_data$age*testing_data$lstat)
abline(model)

#necessary for some god forsaken reason
medvTest = testing_data$medv

predict(model, data.frame(medvTest = c(5)))
predict(model, data.frame(medvTest = c(10)))
predict(model, data.frame(medvTest = c(15)))

prediction <- predict(model, data.frame(medv = testing_data$medv))
prediction

summary(prediction)

##new model
model <- lm(medv ~ .,data = training_data)
model
summary(model)

plot(x=testing_data$medv, y=testing_data$age*testing_data$lstat)
abline(model)

plot(x = log(testing_data$lstat), y = testing_data$medv)
abline(model)
model <- lm(medv ~ .,data = training_data)
model
summary(model)

M = cor(log(x=testing_data$lstat), y=testing_data$medv)

corrplot.mixed(M, order = 'AOE')
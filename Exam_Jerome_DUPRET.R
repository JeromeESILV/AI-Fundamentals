#####Exercise 1
#1
library(tidyverse)
data("marketing", package = "datarium")

#2
dim(marketing)
head(marketing)
#Response: 200 Lines and 4 Columns

#3
LinearM_Marketing <- lm(sales~youtube+facebook, data=marketing)
summary(LinearM_Marketing)

#4
library(Metrics)
# Get the predicted sales values
predicted <- predict(LinearM_Marketing)

# Get the actual sales values
actual <- marketing$sales

# Calculate the MSE
mse(predicted, actual)
#MSE = 4.009781
#####Exercise 2
#1
#Linear regression is used to model the relationship between a continuous dependent variable and one or more independent variables
#Logistic regression is used to model the relationship between a binary dependent variable and one or more independent variables

#2
library(caret)
theme_set(theme_bw())
# Load the data and remove NAs
data("PimaIndiansDiabetes2", package = "mlbench")
PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)
# Inspect the data
sample_n(PimaIndiansDiabetes2, 3)
# Split the data into training and test set
set.seed(123)
training.samples <- PimaIndiansDiabetes2$diabetes %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- PimaIndiansDiabetes2[training.samples, ]
test.data <- PimaIndiansDiabetes2[-training.samples, ]

summary(PimaIndiansDiabetes2)
#3
dim(test.data)
dim(train.data)
#Test 78 Lines 9 Columns
#Train 314 Lines 9 Columns

#4
# Fit a logistic regression model
model <- glm(diabetes ~., data = train.data, family = "binomial")

#5
summary(model)
#From Most important to least Glucose, Pedigree and Mass
#These variables have p-values that are much smaller than the significance level
#of 0.05, indicating that they are statistically significant and likely have a strong
#relationship with the dependent variable.

#6
#probabilities <- model.predict(test.data, type = "response")
probabilities <- predict(model, test.data, type = "response")
#Normally we can generate predictions from a trained model

#7
print(probabilities)
# It contains a set of predicted probabilities for each row of input data.

#8
# Convert the probabilities to binary values using the threshold
binary.values <- ifelse(probabilities > 0.5, "pos", "neg")
print(binary.values)

#9
# Compute the overall accuracy of the model
accuracy(test.data, binary.values)

#10
newModel <- glm(diabetes ~glucose+mass+pregnant, data = train.data, family = "binomial")
summary(newModel)

#11
probabilities2 <- predict(newModel, test.data, type = "response")


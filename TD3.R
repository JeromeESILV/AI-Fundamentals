library(caTools)

setwd(dir = "C:/Users/pc/OneDrive/Uni/R (AI Fundamentals)/SNA")
sna <- read.csv(file = 'Social_Network_Ads.csv')

head(sna)
summary(sna)

sample <- sample.split(sna$Purchased, SplitRatio = 0.75)
train  <- subset(sna, sample == TRUE)
test   <- subset(sna, sample == FALSE)

train_set <- scale(train[c(3,4)], center = TRUE, scale = TRUE)

test_set <- scale(test[c(3,4)], center = TRUE, scale = TRUE)

#fit logistic regression model
model1 <- glm(Purchased ~ Age, family="binomial", data=train)#binomial because we are looking for probabilities

summary(model)



b0 = coef(model1)[1]
b1 =  coef(model1)[2]
p <- function(x) {return(1/(1+exp(-b0-b1*x)))}

plot(train$Age,train$Purchased, col='blue')
points(train$Age,p(train$Age), col = 'red')


curve(p, add = TRUE)

model2 <- glm(Purchased ~ Age+EstimatedSalary, family="binomial", data=train)

summary(model2)

prob_pred = predict(model2, newdata = test[c(3,4)], type="response")
y_pred = ifelse(prob_pred > 0.5, 1,0)
y_pred
test[,5]

cm <-table(y_pred, test[,5])

metrics <- function(CM) {
  acc = (CM[1,1]+CM[2,2])/(CM[1,1]+CM[1,2]+CM[2,1]+CM[2,2])
  spc = CM[1,1]/(CM[1,1]+CM[1,2])
  ses = CM[2,2]/(CM[2,2]+CM[2,1])
  my_list <- list("accuracy" = acc, "specificity" = spc, "sensitivity" = ses)
  return(my_list)}

print(paste("accuracy: ",metrics(cm)$accuracy))

library(ROCR)
score <- prediction(prob_pred,test[,5])
y_pred = ifelse(prob_pred > 0.5, 1, 0)
y_pred
prob_pred

score1 <- prediction(prob_pred,test[,5])
auc1 = as.numeric( performance(score1,"auc")@y.values)
auc1

score2 <- prediction(prob_pred,test[,5])
auc2 = as.numeric( performance(score2,"auc")@y.values)
auc2

plot(performance(score1,"tpr","fpr"), col="green")
abline(0,1,lty=8)

plot(performance(score2,"tpr","fpr"),col="blue",add=T)
abline(0,1,lty=8, col='red')
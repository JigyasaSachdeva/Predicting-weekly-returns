#IDS 575- Assignment 2
#Submitted By: Jigyasa Sachdeva
#UIN: 664791188

#************************************************************************************

#Problem 2- part c

#Loading data 
library(ISLR)
data("Weekly")

#Splitting the data into train and test
str(Weekly)
attach(Weekly)
train<- Weekly[Year <= 2008,]
test<- Weekly[Year > 2008,]

#Confirming the split based on years
unique(train$Year)  #1990-2008
unique(test$Year) #2009, 2010

#Selecting input features in both train and test data 
library(dplyr)
train <- train %>% select(-c(Year, Today))
test <- test %>% select(-c(Year, Today))

#Logistic regression model with 5 Lag and Volume variables as independent variables
model <- glm(Direction ~ ., data = train, family = "binomial")
summary(model)

#Training data: 

#Predictions:
probabilities <- predict(model, data = train, type = "response")
predicted <- ifelse(probabilities > 0.5, "Up", "Down")
predicted <- as.factor(predicted)
#Confusion matrix:
library(caret)
c1 <- confusionMatrix(predicted, train$Direction, positive = "Up")
c1
#Accuracy: 
str(c1)
c1$overall["Accuracy"] #0.5624365


#Test Data: 

#Predictions:
test_probabilities <- predict(model, newdata = test, type = "response")
test_predicted <- ifelse(test_probabilities > 0.5, "Up", "Down")
test_predicted <- as.factor(test_predicted)
#Confusion matrix: 
c2 <- confusionMatrix(test_predicted, test$Direction, positive = "Up")
c2
#Accuracy: 
c2$overall["Accuracy"] #0.4615385


#************************************************************************************

#Problem 2- Part d

#With all 5 variables: 
#F1 Score on Training data: 
train_f1_score <- (2 * c1$byClass["Recall"] * c1$byClass["Precision"]) / (c1$byClass["Recall"] + c1$byClass["Precision"])
train_f1_score #0.6874547 
#F1 Score on Test data: 
test_f1_score <- (2 * c2$byClass["Recall"] * c2$byClass["Precision"]) / (c2$byClass["Recall"] + c2$byClass["Precision"])
test_f1_score #0.3777778

#Storing all evaluation measures in an array for model built with 5 variables: 
all <- c(train_f1_score,  test_f1_score, c1$overall["Accuracy"], c2$overall["Accuracy"])

#Initializing 4 vectors for train f1-score, test f1-score, train accuracy, test accuracy:
t1 <- 0 
t2 <- 0
a1 <- 0
a2 <- 0


#Model with only Lag1
lag1model <- glm(Direction~Lag1, data = train, family = "binomial")

probabilities <- predict(lag1model, data = train, type = "response")
predicted <- ifelse(probabilities > 0.5, "Up", "Down")
predicted <- as.factor(predicted)
c1 <- confusionMatrix(predicted, train$Direction, positive = "Up")

test_probabilities <- predict(lag1model, newdata = test, type = "response")
test_predicted <- ifelse(test_probabilities > 0.5, "Up", "Down")
test_predicted <- as.factor(test_predicted)
c2 <- confusionMatrix(test_predicted, test$Direction, positive = "Up")

#Training data metrics
train_f1_score <- (2 * c1$byClass["Recall"] * c1$byClass["Precision"]) / (c1$byClass["Recall"] + c1$byClass["Precision"])
t1 <- c(t1, train_f1_score)
a1 <- c(a1, c1$overall["Accuracy"]) 

#Testing data metrics
test_f1_score <- (2 * c2$byClass["Recall"] * c2$byClass["Precision"]) / (c2$byClass["Recall"] + c2$byClass["Precision"])
t2 <- c(t2, test_f1_score)
a2 <- c(a2, c2$overall["Accuracy"])


#Model with only Lag2
lag2model <- glm(Direction~Lag2, data = train, family = "binomial")

probabilities <- predict(lag2model, data = train, type = "response")
predicted <- ifelse(probabilities > 0.5, "Up", "Down")
predicted <- as.factor(predicted)
c1 <- confusionMatrix(predicted, train$Direction, positive = "Up")

test_probabilities <- predict(lag2model, newdata = test, type = "response")
test_predicted <- ifelse(test_probabilities > 0.5, "Up", "Down")
test_predicted <- as.factor(test_predicted)
c2 <- confusionMatrix(test_predicted, test$Direction, positive = "Up")

#Training data metrics
train_f1_score <- (2 * c1$byClass["Recall"] * c1$byClass["Precision"]) / (c1$byClass["Recall"] + c1$byClass["Precision"])
t1 <- c(t1, train_f1_score)
a1 <- c(a1, c1$overall["Accuracy"]) 

#Testing data metrics
test_f1_score <- (2 * c2$byClass["Recall"] * c2$byClass["Precision"]) / (c2$byClass["Recall"] + c2$byClass["Precision"])
t2 <- c(t2, test_f1_score)
a2 <- c(a2, c2$overall["Accuracy"])


#Model with only Lag3
lag3model <- glm(Direction~Lag3, data = train, family = "binomial")

probabilities <- predict(lag3model, data = train, type = "response")
predicted <- ifelse(probabilities > 0.5, "Up", "Down")
predicted <- as.factor(predicted)
c1 <- confusionMatrix(predicted, train$Direction, positive = "Up")

test_probabilities <- predict(lag3model, newdata = test, type = "response")
test_predicted <- ifelse(test_probabilities > 0.5, "Up", "Down")
test_predicted <- as.factor(test_predicted)
c2 <- confusionMatrix(test_predicted, test$Direction, positive = "Up")

#Training data metrics
train_f1_score <- (2 * c1$byClass["Recall"] * c1$byClass["Precision"]) / (c1$byClass["Recall"] + c1$byClass["Precision"])
t1 <- c(t1, train_f1_score)
a1 <- c(a1, c1$overall["Accuracy"]) 

#Testing data metrics
test_f1_score <- (2 * c2$byClass["Recall"] * c2$byClass["Precision"]) / (c2$byClass["Recall"] + c2$byClass["Precision"])
t2 <- c(t2, test_f1_score)
a2 <- c(a2, c2$overall["Accuracy"])


#Lag4
lag4model <- glm(Direction~Lag4, data = train, family = "binomial")

probabilities <- predict(lag4model, data = train, type = "response")
predicted <- ifelse(probabilities > 0.5, "Up", "Down")
predicted <- as.factor(predicted)
c1 <- confusionMatrix(predicted, train$Direction, positive = "Up")

test_probabilities <- predict(lag4model, newdata = test, type = "response")
test_predicted <- ifelse(test_probabilities > 0.5, "Up", "Down")
test_predicted <- as.factor(test_predicted)
c2 <- confusionMatrix(test_predicted, test$Direction, positive = "Up")

#Training data metrics
train_f1_score <- (2 * c1$byClass["Recall"] * c1$byClass["Precision"]) / (c1$byClass["Recall"] + c1$byClass["Precision"])
t1 <- c(t1, train_f1_score)
a1 <- c(a1, c1$overall["Accuracy"]) 

#Testing data metrics
test_f1_score <- (2 * c2$byClass["Recall"] * c2$byClass["Precision"]) / (c2$byClass["Recall"] + c2$byClass["Precision"])
t2 <- c(t2, test_f1_score)
a2 <- c(a2, c2$overall["Accuracy"])


#Model with only Lag5:
lag5model <- glm(Direction~Lag5, data = train, family = "binomial")

probabilities <- predict(lag5model, data = train, type = "response")
predicted <- ifelse(probabilities > 0.5, "Up", "Down")
predicted <- as.factor(predicted)
c1 <- confusionMatrix(predicted, train$Direction, positive = "Up")

test_probabilities <- predict(lag5model, newdata = test, type = "response")
test_predicted <- ifelse(test_probabilities > 0.5, "Up", "Down")
test_predicted <- as.factor(test_predicted)
c2 <- confusionMatrix(test_predicted, test$Direction, positive = "Up")

#Training data metrics
train_f1_score <- (2 * c1$byClass["Recall"] * c1$byClass["Precision"]) / (c1$byClass["Recall"] + c1$byClass["Precision"])
t1 <- c(t1, train_f1_score)
a1 <- c(a1, c1$overall["Accuracy"]) 

#Testing data metrics
test_f1_score <- (2 * c2$byClass["Recall"] * c2$byClass["Precision"]) / (c2$byClass["Recall"] + c2$byClass["Precision"])
t2 <- c(t2, test_f1_score)
a2 <- c(a2, c2$overall["Accuracy"])



#Evaluation
eval <- cbind(t1, t2, a1, a2)
colnames(eval) <- c("TrainF1Score", "TestF1Score", "TrainAccuracy", "TestAccuracy")
eval <- eval[-1,]
eval <- as.data.frame(eval)
eval <- rbind(eval, all)
rownames(eval) <- c("Lag1", "Lag2", "Lag3", "Lag4", "lag5", "all")
eval



#************************************************************************************

#Problem 2- part e

library(ROCR)

#Evaluating ROC on train data: 

#Model
all_pred <- predict(model, test, type="response")
all_pref <- prediction(all_pred,test$Direction)
eval <- performance(all_pref,"tpr","fpr")
plot(eval,main="Evaluating Models on Test Data",col='black',lwd=2) 

#Lag1
lag1_pred <- predict(lag1model, test, type="response")
lag1_pref <- prediction(lag1_pred,test$Direction)
eval1 <- performance(lag1_pref,"tpr","fpr")
plot(eval1,col='red',lwd=2, add = TRUE)

#Lag2
lag2_pred <- predict(lag2model, test, type="response")
lag2_pref <- prediction(lag2_pred,test$Direction)
eval2 <- performance(lag2_pref,"tpr","fpr")
plot(eval2,col='blue',lwd=2, add = TRUE)

#Lag3
lag3_pred <- predict(lag3model, test, type="response")
lag3_pref <- prediction(lag3_pred,test$Direction)
eval3 <- performance(lag3_pref,"tpr","fpr")
plot(eval3,col='green',lwd=2, add = TRUE)

#Lag4
lag4_pred <- predict(lag4model, test, type="response")
lag4_pref <- prediction(lag4_pred,test$Direction)
eval4 <- performance(lag4_pref,"tpr","fpr")
plot(eval4,col='orange',lwd=2, add = TRUE)

#Lag5
lag5_pred <- predict(lag5model, test, type="response")
lag5_pref <- prediction(lag5_pred,test$Direction)
eval5 <- performance(lag5_pref,"tpr","fpr")
plot(eval5,col='violet',lwd=2, add = TRUE)

legend('bottomright', c('all', 'lag1', 'lag2', 'lag3','lag4','lag5' ), 
       cex = 0.5, lty=1,
       col=c('black','red','blue','green','orange','violet'))


#Evaluating AUC: 
all_auc <- performance(all_pref, "auc")@y.values
lag1_auc <- performance(lag1_pref, "auc")@y.values
lag2_auc <- performance(lag2_pref, "auc")@y.values
lag3_auc <- performance(lag3_pref, "auc")@y.values
lag4_auc <- performance(lag4_pref, "auc")@y.values
lag5_auc <- performance(lag5_pref, "auc")@y.values
c(all_auc, lag1_auc, lag2_auc, lag3_auc, lag4_auc, lag5_auc)



#Evaluating ROC on Test data: 

#Model
all_pred <- predict(model, train, type="response")
all_pref <- prediction(all_pred,train$Direction)
eval <- performance(all_pref,"tpr","fpr")
plot(eval,main="Evaluating Models on Train Data",col='black',lwd=2) 

#Lag1
lag1_pred <- predict(lag1model, train, type="response")
lag1_pref <- prediction(lag1_pred,train$Direction)
eval1 <- performance(lag1_pref,"tpr","fpr")
plot(eval1,col='red',lwd=2, add = TRUE)

#Lag2
lag2_pred <- predict(lag2model, train, type="response")
lag2_pref <- prediction(lag2_pred,train$Direction)
eval2 <- performance(lag2_pref,"tpr","fpr")
plot(eval2,col='blue',lwd=2, add = TRUE)

#Lag3
lag3_pred <- predict(lag3model, train, type="response")
lag3_pref <- prediction(lag3_pred,train$Direction)
eval3 <- performance(lag3_pref,"tpr","fpr")
plot(eval3,col='green',lwd=2, add = TRUE)

#Lag4
lag4_pred <- predict(lag4model, train, type="response")
lag4_pref <- prediction(lag4_pred,train$Direction)
eval4 <- performance(lag4_pref,"tpr","fpr")
plot(eval4,col='orange',lwd=2, add = TRUE)

#Lag5
lag5_pred <- predict(lag5model, train, type="response")
lag5_pref <- prediction(lag5_pred,train$Direction)
eval5 <- performance(lag5_pref,"tpr","fpr")
plot(eval5,col='violet',lwd=2, add = TRUE)

legend('bottomright', c('all', 'lag1', 'lag2', 'lag3','lag4','lag5' ), 
       cex = 0.5, lty=1, 
       col=c('black','red','blue','green','orange','violet'))


#Area under the curve
all_auc <- performance(all_pref, "auc")@y.values
lag1_auc <- performance(lag1_pref, "auc")@y.values
lag2_auc <- performance(lag2_pref, "auc")@y.values
lag3_auc <- performance(lag3_pref, "auc")@y.values
lag4_auc <- performance(lag4_pref, "auc")@y.values
lag5_auc <- performance(lag5_pref, "auc")@y.values
c(all_auc, lag1_auc, lag2_auc, lag3_auc, lag4_auc, lag5_auc)





#PR Curve

#All
all_pred <- predict(model, test, type="response")
all_pref <- prediction(all_pred,test$Direction)
eval <- performance(all_pref,"prec","rec")
plot(eval,main="PR curves on Test Data",col='black',lwd=2) 

#Lag1
lag1_pred <- predict(lag1model, test, type="response")
lag1_pref <- prediction(lag1_pred,test$Direction)
eval1 <- performance(lag1_pref,"prec","rec")
plot(eval1,col='red',lwd=2, add = TRUE)

#Lag2
lag2_pred <- predict(lag2model, test, type="response")
lag2_pref <- prediction(lag2_pred,test$Direction)
eval2 <- performance(lag2_pref,"prec","rec")
plot(eval2,col='blue',lwd=2, add = TRUE)

#Lag3
lag3_pred <- predict(lag3model, test, type="response")
lag3_pref <- prediction(lag3_pred,test$Direction)
eval3 <- performance(lag3_pref,"prec","rec")
plot(eval3,col='green',lwd=2, add = TRUE)

#Lag4
lag4_pred <- predict(lag4model, test, type="response")
lag4_pref <- prediction(lag4_pred,test$Direction)
eval4 <- performance(lag4_pref,"prec","rec")
plot(eval4,col='orange',lwd=2, add = TRUE)

#Lag5
lag5_pred <- predict(lag5model, test, type="response")
lag5_pref <- prediction(lag5_pred,test$Direction)
eval5 <- performance(lag5_pref,"prec","rec")
plot(eval5,col='violet',lwd=2, add = TRUE)

legend('bottomright', c('all', 'lag1', 'lag2', 'lag3','lag4','lag5' ), 
       cex = 0.5, lty=1,
       col=c('black','red','blue','green','orange','violet'))


#Train

#All
all_pred <- predict(model, train, type="response")
all_pref <- prediction(all_pred,train$Direction)
eval <- performance(all_pref,"prec","rec")
plot(eval,main="PR Curves on Train Data",col='black',lwd=2) 

#Lag1
lag1_pred <- predict(lag1model, train, type="response")
lag1_pref <- prediction(lag1_pred,train$Direction)
eval1 <- performance(lag1_pref,"prec","rec")
plot(eval1,col='red',lwd=2, add = TRUE)

#Lag2
lag2_pred <- predict(lag2model, train, type="response")
lag2_pref <- prediction(lag2_pred,train$Direction)
eval2 <- performance(lag2_pref,"prec","rec")
plot(eval2,col='blue',lwd=2, add = TRUE)

#Lag3
lag3_pred <- predict(lag3model, train, type="response")
lag3_pref <- prediction(lag3_pred,train$Direction)
eval3 <- performance(lag3_pref,"prec","rec")
plot(eval3,col='green',lwd=2, add = TRUE)

#Lag4
lag4_pred <- predict(lag4model, train, type="response")
lag4_pref <- prediction(lag4_pred,train$Direction)
eval4 <- performance(lag4_pref,"prec","rec")
plot(eval4,col='orange',lwd=2, add = TRUE)

#Lag5
lag5_pred <- predict(lag5model, train, type="response")
lag5_pref <- prediction(lag5_pred,train$Direction)
eval5 <- performance(lag5_pref,"prec","rec")
plot(eval5,col='violet',lwd=2, add = TRUE)

legend('bottomright', c('all', 'lag1', 'lag2', 'lag3','lag4','lag5' ), cex = 0.5, lty=1, col=c('black','red','blue','green','orange','violet'))


install.packages("caTools")
library(caTools)
auc <- trapz(recall, precision)



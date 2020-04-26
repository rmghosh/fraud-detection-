install.packages("ranger")
install.packages("caret")
install.packages("data.table")
install.packages("ggplot2")
install.packages("lattice")
install.packages("caTools")

library(ranger)
library(caret)
library(data.table)
creditcard_data <- read.csv("creditcard.csv")

#Data Exploraration
dim(creditcard_data)
head(creditcard_data,6)
tail(creditcard_data,6)
table(creditcard_data$Class)
summary(creditcard_data$Amount)
names(creditcard_data)
var(creditcard_data$Amount)
sd(creditcard_data$Amount)

#Data Manipulation
creditcard_data$Amount=scale(creditcard_data$Amount)
NewData=creditcard_data[,-c(1)]
head(NewData)

#Data Modelling
library(caTools)
set.seed(123)
data_sample = sample.split(NewData$Class,SplitRatio=0.80)
train_data = subset(NewData,data_sample==TRUE)
test_data = subset(NewData,data_sample==FALSE)
dim(train_data)
dim(test_data)

#Logistic Modelling

Logistic_Model=glm(Class~.,test_data,family=binomial())
summary(Logistic_Model)
plot(Logistic_Model)

#ROC Curve
install.packages("pROC")
library(pROC)
lr.predict <- predict(Logistic_Model,test_data, probability = TRUE)
auc.gbm = roc(test_data$Class, lr.predict, plot = TRUE, col = "blue")

#Decision-Tree Model

install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
decisionTree_model <- rpart(Class ~ . , creditcard_data, method = 'class')
predicted_val <- predict(decisionTree_model, creditcard_data, type = 'class')
probability <- predict(decisionTree_model, creditcard_data, type = 'prob')
rpart.plot(decisionTree_model)

#Artificial NN

install.packages("neuralnet")
library(neuralnet)
ANN_model =neuralnet (Class~.,train_data,linear.output=FALSE)
plot(ANN_model)
predANN=compute(ANN_model,test_data)
resultANN=predANN$net.result
resultANN=ifelse(resultANN>0.5,1,0)

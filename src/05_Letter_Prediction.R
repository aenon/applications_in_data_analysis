#Install and run relevant libraries
install.packages('rpart')
install.packages('rpart.plot')
install.packages('randomForest')
library(randomForest)
library(caTools)
library(rpart)
library(rpart.plot)

#Import and summarize the dataset
letters = read.csv('Letters.csv')
str(letters)
summary(letters)

#Creating new IsB variable
letters$isB = as.factor(letters$Letter == "B")

#Split the dataset into a train and test data set
set.seed(1234)
split = sample.split(letters, SplitRatio = 0.5)
train = subset(letters, split == TRUE)
test = subset(letters, split == FALSE)

#Build a baseline model and determine the accuracy
table(test$isB)[1]/nrow(test)

#Create a Train Cart
TrainCart = rpart(isB ~ . - Letter, data = train, method = 'class')
prp(TrainCart)

#Predict the test set
PredictCart = predict(TrainCart, newdata = test, type='class')

#Confusion Matrix
table(test$isB,PredictCart)

#Determine the accuracy of the test set for the CART test
(table(test$isB,PredictCart)[1] + table(test$isB,PredictCart)[4])/nrow(test)

#This command simplifies the above two answers and gives more details
confusionMatrix(PredictCart,test$isB)

#Create a Random Forest
set.seed(1234)
ranForest = randomForest(isB ~ . -Letter,data = train)

#Predict the accuracy of the test set
PredictRanForest = predict(ranForest, newdata = test, type='class')
table(test$isB,PredictRanForest)
(table(test$isB,PredictRanForest)[1] + table(test$isB,PredictRanForest)[4])/nrow(test)

#This command simplifies the above two answers and gives more details
confusionMatrix(PredictRanForest,test$isB)

#Baseline Accuracy for the test set
table(train$Letter)
table(test$Letter)
#table(test$Letter)[1]/nrow(test)
#table(test$Letter)[2]/nrow(test)
table(test$Letter)[3]/nrow(test)
#table(test$Letter)[4]/nrow(test)
#Yes it is useful because it gives a representation of the dataset right now

#Classification Tree to predict Letter
TrainCart1 = rpart(Letter ~ .-isB, data = train, method = 'class')
prp(TrainCart1)

#Predict the test set
PredictCart1 = predict(TrainCart1, newdata = test, type='class')

#Confusion Matrix
table(test$Letter,PredictCart1)

#Determine the accuracy of the test set for the Predicted Cart
sum(table(test$Letter,PredictCart1)[1,1] + table(test$Letter,PredictCart1)[2,2] +
      table(test$Letter,PredictCart1)[3,3] + table(test$Letter,PredictCart1)[4,4]) / nrow(test)

#Confusion Matrix and Accuracy
confusionMatrix(PredictCart1,test$Letter)

#Create a Random Forest to predict Letter
set.seed(1234)
ranForest1 = randomForest(Letter ~ . -isB,data = train)
print(ranForest1)

#Predict the Random Forest for the Test Set
PredictRanForest1 = predict(ranForest1, newdata = test, type='class')
table(test$Letter,PredictRanForest1)

#Determine the accuracy of the Random Forest Model
sum(table(test$Letter,PredictRanForest1)[1,1] + table(test$Letter,PredictRanForest1)[2,2] + table(test$Letter,PredictRanForest1)[3,3] + table(test$Letter,PredictRanForest1)[4,4]) / nrow(test)

#Confusion Matrix and Accuracy
confusionMatrix(PredictRanForest1,test$Letter)

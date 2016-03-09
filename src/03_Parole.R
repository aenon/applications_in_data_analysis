# Import libraries
library(ggplot2)
library(dplyr)
library(reshape2)
library(caTools)
library(ROCR)

#Read the dataset
parole = read.csv("Parole.csv")
parole$Male=as.factor(parole$Male)
parole$RaceWhite=as.factor(parole$RaceWhite)
parole$MultipleOffenses=as.factor(parole$MultipleOffenses)
parole$Violator=as.factor(parole$Violator)

# Understand data struture and summarize it
str(parole)
summary(parole)

#Determine number of parolees and percentage that violated their parole
violators = table(parole$Violator)
numParolees = nrow(parole)
numParolees
violators[2]/numParolees


#Randomly spliting the dataset
set.seed(123)
split=sample.split(parole$Violator,SplitRatio = 0.7)
train=subset(parole,split==TRUE)
test=subset(parole,split==FALSE)
train_ind <- sample(nrow(parole), floor(nrow(parole)) * 0.7)
train <- parole[train_ind, ]
test <- parole[-train_ind, ]

#First logistic regression model containing all independent variables
model = glm(Violator ~ Male + RaceWhite + Age + State + TimeServed + MaxSentence + MultipleOffenses + Crime, data = train, family = binomial)
summary(model)

#Predict Probabilty of specific parolee
newlogregmodel = data.frame(Male = "1", RaceWhite = "1", Age =50, TimeServed = 3, State = "Other" , MaxSentence = 12, MultipleOffenses = "0", Crime = 'Larceny')
predict(model, newlogregmodel, type= 'response')

#Compute the probablity for the test set
probpredict = predict(model, newdata = test, type = 'response')

confusionMatrix = table(test$Violator, probpredict >= 0.5)
#False Positive = 0:True 9/181
#False Negative = 1:False 17/189
#Overall Accuracy = 172+5/203

#False Positive = 0:True 9/181
confusionMatrix[3]/sum(confusionMatrix[1,])

#False Negative = 1:False 17/189
confusionMatrix[2]/sum(confusionMatrix[,"FALSE"])

#Overall Accuracy = 172+5/203
sum(confusionMatrix[1],confusionMatrix[4])/sum(confusionMatrix)

#Baseline Model
table=table(test$Violator)
table[1]/sum(table)

#Parole Boards want the models false negative to be less. We should reduce the threshold.

#AUC
ROCRpred=prediction(probpredict,test$Violator)
ROCRpref=performance(ROCRpred,"tpr","fpr")
auc=as.numeric(performance(ROCRpred,"auc")@y.values)
auc
#Interpretation of AUC - Given a random parolee from the dataset who actually violated the terms of the parole and a person who did not violate the terms, the AUC is the indications of the percentage of times our model will classify which is which correctly.

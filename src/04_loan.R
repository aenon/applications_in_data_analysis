library(ggplot2)
library(dplyr)
library(reshape2)
library(caTools)
library(ROCR)

#Read data and summarize
loans = read.csv("Loans.csv")
summary(loans)
str(loans)

#Randomly spliting the dataset
set.seed(144)
split=sample.split(loans$NotFullyPaid,SplitRatio = 0.7)
train=subset(loans,split==TRUE)
test=subset(loans,split==FALSE)


# Accuracy of baseline on testset
acc=table(test$NotFullyPaid)[1]/nrow(test)
acc

#Logistic Regression model
model1 <- glm(NotFullyPaid ~ CreditPolicy + Purpose + IntRate + Installment + LogAnnualInc + Dti + Fico + DaysWithCrLine + RevolBal + RevolUtil + InqLast6mths + Delinq2yrs + PubRec, data=train, family=binomial)
summary(model1)

#LogitA-LogitB
diffa_b = -10*model1$coefficients[13]
diffa_b

#Prediction for Test set
probpredict = predict(model1, newdata = test, type = 'response')
test$PredictedRisk=probpredict
confusionMatrix = table(test$NotFullyPaid, probpredict >= 0.5)

# Accuracy for test set
modelacc=sum(confusionMatrix[1],confusionMatrix[4])/sum(confusionMatrix)
modelacc

# AUC
ROCRpred=prediction(probpredict,test$NotFullyPaid)
ROCRpref=performance(ROCRpred,"tpr","fpr")
auc=as.numeric(performance(ROCRpred,"auc")@y.values)
auc

# Logistic Regression using only Interest rate
model2 <- glm(NotFullyPaid ~ IntRate, data=train, family=binomial)
summary(model2)

#Predictions on test set
probpredict2 = predict(model2, newdata = test, type = 'response')
# Highest probability of loan not paid back
max(probpredict2)
# Number of loans not paid back
notpaid = table(probpredict2 >= 0.5)
notpaid
#AUC
ROCRpred2=prediction(probpredict2,test$NotFullyPaid)
ROCRpref2=performance(ROCRpred2,"tpr","fpr")
auc2=as.numeric(performance(ROCRpred2,"auc")@y.values)
auc2

#Make a correlation plot to show correlation among variables

# How much investment pay
invreturn= 10*exp(0.06*3)
invreturn
profitifpaid=invreturn-10
profitifnotpaid=-10 
profitifpaid
profitifnotpaid

#Profit column and max profit computation
loans$Profit=(1-loans$NotFullyPaid)*exp(loans$IntRate*3) - 1
max(loans$Profit)
train$Profit=(1-train$NotFullyPaid)*exp(train$IntRate*3) - 1
test$Profit=(1-test$NotFullyPaid)*exp(test$IntRate*3) - 1
max(test$Profit)

#High interest rate dataset
HighInterest=subset(test, test$IntRate >= 0.15)
mean(HighInterest$Profit) # Avg profit
proportionnotpaid= table(HighInterest$NotFullyPaid)[2]/nrow(HighInterest) # proportion not paid
proportionnotpaid

# Dataset of smaller risk
SelectedLoans = HighInterest[order(HighInterest$PredictedRisk),]
SelectedLoans = SelectedLoans[1:100,]
#Profit of investor
sum(SelectedLoans$Profit)
# Number of loans not paid back
table(SelectedLoans$NotFullyPaid)[2]

#Model does not take into account such as financial downturn when there might be a greate default rates on loans.


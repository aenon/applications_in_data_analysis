#Install necessary libraries
library(randomForest)
library(caTools)
library(rpart)
library(rpart.plot)
library(caret) # Mihir's edit

#Read in the dataset
data = read.csv('StateData.csv')
summary(data)
str(data)

#Build Linear Regression with 7 independent variables
linReg = lm(LifeExp ~ Population + Murder + Frost + Income + Illiteracy + Area + HighSchoolGrad, data = data)
summary(linReg)

#Build Linear Regression with 4 independent variables
linReg2 = lm(LifeExp ~ Population + Murder + Frost + HighSchoolGrad, data = data)
summary(linReg2)

#Mihir edits from here
fitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 10)
TrainCart = train(LifeExp ~ Population + Murder + Frost + Income + Illiteracy + Area + HighSchoolGrad, data = data, method = 'anova', control=fitControl)
# Mihir edits until above

prp(TrainCart)
TrainCart$frame$complexity
TrainCart$control$cp
TrainCart$control

printcp(TrainCart)
rsq.rpart(TrainCart)
plotcp(TrainCart)
p = prune(TrainCart,cp=0.04379453)
plot(p)
TrainCart$cptable[which.min(TrainCart$cptable[,"xerror"]),"CP"]


#Computing Predictions of life expectancies
PredictCart = predict(TrainCart)
summary(PredictCart)

#Determine the R^2 of the predictions
sse=sum((PredictCart - data$LifeExp)^2)
sst=sum((data$LifeExp-(mean(PredictCart)))^2)
rsquare=1-(sse/sst)
rsquare


#Changing the parameters to prevent overfitting
trainCart2 = rpart(LifeExp ~ Population + Murder + Frost + Income + Illiteracy + Area + HighSchoolGrad, data = data, method = 'anova', cp=0.01)
prp(trainCart2)
predictCart2 = predict(trainCart2)

#Calculate R^2
sse=sum((predictCart2 - data$LifeExp)^2)
sst=sum((data$LifeExp-(mean(predictCart2)))^2)
rsquare=1-(sse/sst)
rsquare



#Random Forest Model
set.seed(1234)
ranForest = randomForest(LifeExp ~ Population + Murder + Frost + Income + Illiteracy + Area + HighSchoolGrad, data = data, nodesize=25, ntree=3000)

predictRanForest = predict(ranForest)
summary(predictRanForest)


sse=sum((predictRanForest - data$LifeExp)^2)
sst=sum((data$LifeExp-(mean(predictRanForest)))^2)
rsquare=1-(sse/sst)
rsquare

# Code for validation
set.seed(123)
split=sample.split(data$LifeExp,SplitRatio = 0.8)
train=subset(data,split==TRUE)
validation=subset(data,split==FALSE)
rsquare=c(1:100)
r_new=vector()
for(i in seq(1,25,by=1)){
  trainCart = rpart(LifeExp ~ Population + Murder + Frost + Income + Illiteracy + Area + HighSchoolGrad, data = train, minbucket=i)
  PredictCart = predict(trainCart,newdata=validation)
  sse=sum((PredictCart - validation$LifeExp)^2)
  sst=sum((validation$LifeExp-(mean(train$LifeExp)))^2)
  r_new=c(r_new,1-(sse/sst))
}
ropt=which.max(r_new)
bestmodel = rpart(LifeExp ~ Population + Murder + Frost + Income + Illiteracy + Area + HighSchoolGrad, data = data, minbucket=ropt)

#Computing Predictions of life expectancies
PredictCart = predict(bestmodel,newdata = data)

#Determine the R^2 of the predictions
sse=sum((PredictCart - data$LifeExp)^2)
sst=sum((data$LifeExp-(mean(data$LifeExp)))^2)
rsquare = 1-(sse/sst)
rsquare

#Best Cp
set.seed(123)
numfolds=trainControl(method="cv",number=5)
cpGrid=expand.grid(cp=seq(0.001,0.3,0.001))
train(LifeExp ~ Population + Murder + Frost + Income + Illiteracy + Area + HighSchoolGrad, data = train, minbucket=ropt, method = "rpart", trControl = numfolds, tuneGrid = cpGrid)

bestcp=rpart(LifeExp ~ Population + Murder + Frost + Income + Illiteracy + Area + HighSchoolGrad, data = data, minbucket=ropt , cp=0.151)
PredictCart = predict(bestcp,newdata=data)
sse=sum((PredictCart - data$LifeExp)^2)
sst=sum((data$LifeExp-(mean(data$LifeExp)))^2)
1-(sse/sst)


set.seed(1234)
numfolds=trainControl(method="cv",number=5)
mtryGrid = expand.grid(mtry = seq(1,11,1))
train(LifeExp ~ Population + Murder + Frost + Income + Illiteracy + Area + HighSchoolGrad, data = data, minbucket=ropt, method = "rf", trControl = numfolds, tuneGrid = mtryGrid , ntree = 500)
bestmtry=randomForest(LifeExp ~ Population + Murder + Frost + Income + Illiteracy + Area + HighSchoolGrad, data = data, mtry=6, ntree=500)
predictRanForest = predict(bestmtry, data=data)
sse=sum((predictRanForest - data$LifeExp)^2)
sst=sum((data$LifeExp-(mean(predictRanForest)))^2)
rsquare=1-(sse/sst)
rsquare

set.seed(1234)
train(LifeExp ~ Population + Murder + Frost + Income + Illiteracy + Area + HighSchoolGrad, data = data, minbucket=ropt, method = "rf", trControl = numfolds, tuneGrid = mtryGrid, ntree = 1000)
bestmtry=randomForest(LifeExp ~ Population + Murder + Frost + Income + Illiteracy + Area + HighSchoolGrad, data = data, mtry=3, ntree=1000)
predictRanForest = predict(bestmtry, data=data)
sse=sum((predictRanForest - data$LifeExp)^2)
sst=sum((data$LifeExp-(mean(predictRanForest)))^2)
rsquare=1-(sse/sst)
rsquare

set.seed(1234)
train(LifeExp ~ Population + Murder + Frost + Income + Illiteracy + Area + HighSchoolGrad, data = data, minbucket=ropt, method = "rf", trControl = numfolds, tuneGrid = mtryGrid, ntree = 2000)
bestmtry=randomForest(LifeExp ~ Population + Murder + Frost + Income + Illiteracy + Area + HighSchoolGrad, data = data, mtry=3, ntree=2000)
predictRanForest = predict(bestmtry, data=data)
sse=sum((predictRanForest - data$LifeExp)^2)
sst=sum((data$LifeExp-(mean(predictRanForest)))^2)
rsquare=1-(sse/sst)
rsquare

set.seed(1234)
train(LifeExp ~ Population + Murder + Frost + Income + Illiteracy + Area + HighSchoolGrad, data = data, minbucket=ropt, method = "rf", trControl = numfolds, tuneGrid = mtryGrid, ntree = 5000)
bestmtry=randomForest(LifeExp ~ Population + Murder + Frost + Income + Illiteracy + Area + HighSchoolGrad, data = data, mtry=2, ntree=5000)
predictRanForest = predict(bestmtry, data=data)
sse=sum((predictRanForest - data$LifeExp)^2)
sst=sum((data$LifeExp-(mean(predictRanForest)))^2)
rsquare=1-(sse/sst)
rsquare

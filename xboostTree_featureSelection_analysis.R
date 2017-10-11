#Which features are more important than others?

install.packages("stringi")
library(stringi)
library(data.table)
library(rpart)
library(rpart.plot)
library(caret)
library(xgboost)
library(pROC)

source("C://Users//chris//OneDrive//Documentos//GitHub//photinus-analytics//loadAnswers.R");
dataf <- loadAnswers();

#Compute column to label answers as correct/wrong
dataf$Answer.correct<- ((dataf$TP==1) + (dataf$TN==1));

featuresdf<- data.frame(dataf$Answer.duration,dataf$Answer.confidence,dataf$Answer.difficulty,stri_length(dataf$Answer.explanation), 
                        dataf$Worker.age,dataf$Worker.yearsOfExperience,dataf$Worker.score,dataf$Worker.profession,
                        dataf$Code.LOC,dataf$Code.complexity,dataf$Answer.correct);
colnames(featuresdf) <- c("Answer.duration","Answer.confidence","Answer.difficulty","Answer.explanationSize", 
                          "Worker.age","Worker.yearsOfExperience","Worker.score","Worker.profession",
                          "Code.LOC","Code.complexity","Answer.correct");

  
#Scramble the dataset before extracting the training set.
set.seed(8850);
g<- runif((nrow(featuresdf))); #generates a random distribution
featuresdf <- featuresdf[order(g),];

#################################################
#Select train and test data
totalData = dim(featuresdf)[1];
trainingSize = trunc(totalData * 0.7);
startTestIndex = totalData - trainingSize;
endTestIndex = totalData;

trainingData<- as.data.frame(featuresdf[1:trainingSize,]);
testingData<-as.data.frame(featuresdf[startTestIndex:endTestIndex,]);

cv <- createFolds(trainingData[,1:10], k = 10);
# Control
ctrl <- trainControl(method = "cv",index = cv);

#################################################
#Train model

  # https://www.analyticsvidhya.com/blog/2016/01/xgboost-algorithm-easy-steps/
  # https://medium.com/applied-data-science/new-r-package-the-xgboost-explainer-51dd7d1aa211

xgb.train.data = matrix(data.matrix(trainingData[,1:10]), label = trainingData[,11], missing = NA)

param <- list(objective = "binary:logistic", base_score = 0.5)
xgboost.cv = xgb.cv(param=param, data = xgb.train.data, folds = cv, nrounds = 1500, early_stopping_rounds = 100, metrics='auc')
best_iteration = xgboost.cv$best_iteration

xgb.model <- xgboost(param =param,  data = xgb.train.data, nrounds=best_iteration)

https://medium.com/applied-data-science/new-r-package-the-xgboost-explainer-51dd7d1aa211

#Load libraries
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(knitr)

#Download the data
if(!file.exists("pml-training.csv")){download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile = "pml-training.csv")}

if(!file.exists("pml-testing.csv")){download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile = "pml-testing.csv")}


#Read the training data and replace empty values by NA
trainingDataSet<- read.csv("pml-training.csv", sep=",", header=TRUE, na.strings = c("NA","",'#DIV/0!'))
testingDataSet<- read.csv("pml-testing.csv", sep=",", header=TRUE, na.strings = c("NA","",'#DIV/0!'))
dim(trainingDataSet)
dim(testingDataSet)

#clean the Data

trainingDataSet <- trainingDataSet[,(colSums(is.na(trainingDataSet)) == 0)]
dim(trainingDataSet)

testingDataSet <- testingDataSet[,(colSums(is.na(testingDataSet)) == 0)]
dim(testingDataSet)

#Preprocess the data
numericalsIdx <- which(lapply(trainingDataSet, class) %in% "numeric")

preprocessModel <-preProcess(trainingDataSet[,numericalsIdx],method=c('knnImpute', 'center', 'scale'))
pre_trainingDataSet <- predict(preprocessModel, trainingDataSet[,numericalsIdx])
pre_trainingDataSet$classe <- trainingDataSet$classe

pre_testingDataSet <-predict(preprocessModel,testingDataSet[,numericalsIdx])

#Removing the non zero variables
nzv <- nearZeroVar(pre_trainingDataSet,saveMetrics=TRUE)
pre_trainingDataSet <- pre_trainingDataSet[,nzv$nzv==FALSE]

nzv <- nearZeroVar(pre_testingDataSet,saveMetrics=TRUE)
pre_testingDataSet <- pre_testingDataSet[,nzv$nzv==FALSE]


#Validation Set
set.seed(12031987)
idxTrain<- createDataPartition(pre_trainingDataSet$classe, p=3/4, list=FALSE)
training<- pre_trainingDataSet[idxTrain, ]
validation <- pre_trainingDataSet[-idxTrain, ]
dim(training) ; dim(validation)


#Train Model
library(randomForest)
modFitrf <- train(classe ~., method="rf", data=training, trControl=trainControl(method='cv'), number=5, allowParallel=TRUE, importance=TRUE )
modFitrf


predValidRF <- predict(modFitrf, validation)
confus <- confusionMatrix(validation$classe, predValidRF)
confus$table

accur <- postResample(validation$classe, predValidRF)
modAccuracy <- accur[[1]]
modAccuracy


out_of_sample_error <- 1 - modAccuracy
out_of_sample_error

#Application of this model on the 20 test cases provided
pred_final <- predict(modFitrf, pre_testingDataSet)
pred_final






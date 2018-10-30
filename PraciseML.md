---
title: "Prediction Assignment"
author: "Suresh Rajamanickam"
date: "October 30, 2018"
output: html_document
---

## Loading the required packages
```{r Load}
require(caret)
require(dplyr)
require(ggplot2)
```

## Reading in the training and test datasets
```{r read}
training <- read.csv(file = "pml-training.csv", header = TRUE, na.strings = c("NA", ""))
testing <- read.csv(file = "pml-testing.csv", header = TRUE, na.strings = c("NA", ""))
```

## Examining the structure of the training dataset
```{r examining}
str(training)
```

## Selecting the required variables for modelling 
```{r selection}
training <- select(.data = training, c("new_window", "num_window", "roll_belt", "pitch_belt", "yaw_belt", "roll_arm", "pitch_arm", "yaw_arm", "roll_dumbbell", "pitch_dumbbell", "yaw_dumbbell", "roll_forearm", "pitch_forearm", "yaw_forearm", "classe"))
```

## Exploratory data analysis
```{r explore}
table(training$classe)
ggplot(data = training) + geom_bar(mapping = aes(x = classe, fill = new_window))
ggplot(data = training) + geom_boxplot(mapping = aes(x = classe, y = num_window))
```

## Building a Classification Tree Model
```{r tree}
set.seed(1)
model_tree <- train(form = classe ~ ., data = training, method = "rpart")
model_tree
table(training$classe, predict(object = model_tree, newdata = training))
```
As can be seen from the table above, a classification tree model does not appear to be a good model considering the **Accuracy of approximately 52%**.

## Building a Bootstrap Aggregating (Bagging) Model
```{r bagging}
set.seed(2)
model_bagging <- train(form = classe ~ ., data = training, method = "treebag")
model_bagging
table(training$classe, predict(object = model_bagging, newdata = training))
```
A bootstrap aggregating model as can be seen from the table above appears to be a good model for the data and this is further confirmed by the high **Accuracy of approximately 100%**.

## Building a Boosting Model
```{r boosting}
set.seed(3)
model_boosting <- train(form = classe ~ ., data = training, method = "gbm", verbose = FALSE)
model_boosting
table(training$classe, predict(object = model_boosting, newdata = training))
```
Just like the bagging model, the boosting model also has a very high **Accuracy of approximately 99%** and it can also be seen in the table above.

## Predicting with the Bagging and Boosting Model
```{r predict}
predict_bagging <- predict(object = model_bagging, newdata = testing)
predict_bagging
predict_boosting <- predict(object = model_boosting, newdata = testing)
predict_boosting
```
As can be seen from the predictions above, the bagging and boosting models predictions are the same for the 20 cases.

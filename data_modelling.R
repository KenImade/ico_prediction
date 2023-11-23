# DATA MODELLING ----

set.seed(128)

# libraries
library(dplyr)
library(caret)
library(gmodels)
library(cluster)
library(pROC)
library(ROCR)

# load dataset
data <- read.csv("Data/data_preprocessed.csv")

# Factoring target variable
data$success <- factor(data$success, levels = c("No", "Yes"))
table(data$success)

# splitting data
training_indices <- createDataPartition(data$success, p = 0.8, 
                                        list = FALSE)
train_data <- data[training_indices,]
test_data <- data[-training_indices,]

train_data <- train_data[,1:11]
test_data <- test_data[,1:11]
summary(train_data[,1:11])

# Measures ----
accuracy <- function(actual, predicted) {
  correct <- sum(actual == predicted)
  total <- length(actual)
  accuracy <- correct / total
  return(accuracy)
}

precision <- function(actual, predicted, positive_label) {
  true_positive <- sum(actual == positive_label & predicted == positive_label)
  false_positive <- sum(actual != positive_label & predicted == positive_label)
  precision <- true_positive / (true_positive + false_positive)
  return(precision)
}

# Controls
ctrl_1 <- trainControl(method = "cv", number = 10)
ctrl_2 <- trainControl(method = "CV", number = 30)
ctrl_3 <- trainControl(method = "CV", number = 50)

# KNN ----
knn_model <- train(success ~ .,
                   data = train_data,
                   method = "knn",
                   trControl = ctrl_3,
                   preProcess = c("center", "scale"))

knn_predict <- predict(knn_model, newdata = test_data)

CrossTable(test_data$success, knn_predict, dnn=c('actual', 'predict'),
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE)
knn_accuracy <- accuracy(test_data$success, knn_predict)
knn_precision <- precision(test_data$success, knn_predict, "Yes")
knn_sensitivity <- sensitivity(knn_predict, test_data$success, positive = "Yes")
knn_specificity <- specificity(knn_predict, test_data$success, negative = "No")


# Naive Bayes ----
nb_model <- train(success ~., data = train_data, method = "naive_bayes",
                  trControl = ctrl_3)

nb_predict <- predict(nb_model, newdata = test_data)

CrossTable(nb_predict, test_data$success, dnn=c('predict', 'actual'),
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE)
nb_accuracy <- accuracy(test_data$success, nb_predict)
nb_precision <- precision(test_data$success, nb_predict, "Yes")
nb_sensitivity <- sensitivity(nb_predict, test_data$success, positive = "Yes")
nb_specificity <- specificity(nb_predict, test_data$success, negative = "No")

# Decision Trees ----
dt_model <- train(success ~., data = train_data, method = "rpart",
                  trControl = ctrl_3)

dt_predict <- predict(dt_model, newdata = test_data)

CrossTable(dt_predict, test_data$success, dnn=c('predict', 'actual'),
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE)
dt_accuracy <- accuracy(test_data$success, dt_predict)
dt_precision <- precision(test_data$success, dt_predict, "Yes")
dt_sensitivity <- sensitivity(dt_predict, test_data$success, positive = "Yes")
dt_specificity <- specificity(dt_predict, test_data$success, negative = "No")

# SVM ----
svm_model <- train(success ~., data = train_data, method = "svmRadial",
                   trainControl = ctrl_3)

svm_predict <- predict(svm_model, newdata = test_data)

CrossTable(svm_predict, test_data$success, dnn=c('predict', 'actual'),
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE)
svm_accuracy <- accuracy(test_data$success, svm_predict)
svm_precision <- precision(test_data$success, svm_predict, "Yes")
svm_sensitivity <- sensitivity(svm_predict, test_data$success, positive = "Yes")
svm_specificity <- specificity(svm_predict, test_data$success, negative = "No")

# Logistic Regression ----
# training
lr_model <- train(success ~ ., data = train_data, method = "glm", 
                   family = "binomial", trControl = ctrl_3)

# obtain predicted values
lr_predict <- predict(lr_model, test_data)


CrossTable(lr_predict, test_data$success, dnn=c('predict', 'actual'),
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE)

lr_accuracy <- accuracy(test_data$success, lr_predict)
lr_precision <- precision(test_data$success, lr_predict, "Yes")
lr_sensitivity <- sensitivity(lr_predict, test_data$success, positive = "Yes")
lr_specificity <- specificity(lr_predict, test_data$success, negative = "No")
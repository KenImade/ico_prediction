# DATA MODELLING - Neural Networks ----

# libraries ----
library(neuralnet)
library(caret)
library(tidyverse)
library(Metrics)

# Set seed
set.seed(123)

# load dataset
data <- read.csv("Data/normalized_data.csv")
str(data)

# Factoring target variable
data$success <- factor(data$success, levels = c("No", "Yes"))
table(data$success)

# splitting data
training_indices <- createDataPartition(data$success, p = 0.8, 
                                        list = FALSE)
train_data <- data[training_indices, ]
test_data <- data[-training_indices, ]

# training
ann_model <- neuralnet(formula = success ~., data = train_data, train )

# obtain predicted values
ann_predict <- predict(ann_model, select(test_data, -success))

# Convert ann_predict to binary labels
ann_predict_labels <- apply(ann_predict, 1, function(row) {
  ifelse(row[1] > row[2], "No", "Yes")
})

# Calculate accuracy
confusionMatrix(table(ann_predict_labels, test_data$success))

# optimizing
ann_model_opt <- neuralnet(formula = success ~., data = train_data, 
                           hidden = c(20, 10, 2))

ann_predict_opt <- predict(ann_model_opt, select(test_data, -success)) 

# Convert ann_predict to binary labels
ann_predict_opt_labels <- apply(ann_predict_opt, 1, function(row) {
  ifelse(row[1] > row[2], "No", "Yes")
})

# Calculate accuracy
confusionMatrix(table(ann_predict_opt_labels, test_data$success))

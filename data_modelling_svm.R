# DATA MODELLING - SVM ----

# libraries
library(caret)
library(kernlab)

# Set seed
set.seed(123)

# load dataset
data <- read.csv("Data/normalized_data.csv")

# Factoring target variable
data$success <- factor(data$success, levels = c("No", "Yes"))
table(data$success)

# splitting data
training_indices <- createDataPartition(data$success, p = 0.8, 
                                        list = FALSE)
train_data <- data[training_indices, ]
test_data <- data[-training_indices, ]

train_data <- train_data[, !apply(train_data, 2, 
                                  function(x) length(unique(x))) == 1]

# training
svm_model <- ksvm(success ~., data = train_data, kernel = 'polydot')

# Prediction
svm_pred <- predict(svm_model, select(test_data, -success))

CrossTable(svm_pred, test_data$success, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual' ))

# Optimizing
svm_model_optimized <- ksvm(success ~., data = train_data, kernel = 'rbfdot')

svm_pred_optimized <- predict(svm_model_optimized, select(test_data, -success))

CrossTable(svm_pred_optimized, test_data$success, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual' ))

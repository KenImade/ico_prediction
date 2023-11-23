# DATA MODELLING - Decision Trees ----

# libraries
library(C50)
library(gmodels)
library(caret)

# Set seed for reproductubility
set.seed(123)

# load dataset
data <- read.csv("Data/data_preprocessed.csv")

# Factoring target variable
data$success <- factor(data$success, levels = c("No", "Yes"))
table(data$success)

# splitting data
training_indices <- createDataPartition(data$success, p = 0.8, 
                                        list = FALSE)
train_data <- data[training_indices, ]
test_data <- data[-training_indices, ]

# train model
tree_model <- C5.0(select(train_data, -success), train_data$success)

# Summary
summary(tree_model)

# Evaluation
ico_pred <- predict(tree_model, test_data)

CrossTable(ico_pred, test_data$success, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual' ))

# Improving model performance
ico_boost <- C5.0(select(train_data, -success), train_data$success, trials = 15)

summary(ico_boost)

ico_boost_pred <- predict(ico_boost, test_data)

CrossTable(ico_boost_pred, test_data$success, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual' ))

# DATA MODELLING - Naive Bayes ----

# libraries
library(dplyr)
library(tm)
library(SnowballC)
library(class)
library(caret)
library(e1071)
library(gmodels)
library(ggplot2)

# set the seed for reproducibility
set.seed(123)

# import data
data <- read.csv("Data/data_preprocessed.csv")

# structure of data
str(data)
summary(data)
df <- data

# convert counts to categorical variable
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

df_1 <- df[, 1:11]

# Discretization of numerical columns
str(df_1)
df_1$success <- factor(df_1$success, levels = c("No", "Yes"))
df_1$hasVideo <- lapply(df_1$hasVideo, convert_counts)
df_1$hasVideo <- factor(df_1$hasVideo, levels = c("No", "Yes"))
df_1$hasGithub <- lapply(df_1$hasGithub, convert_counts)
df_1$hasGithub <- factor(df_1$hasGithub, levels = c("No", "Yes"))
df_1$hasReddit <- lapply(df_1$hasReddit, convert_counts)
df_1$hasReddit <- factor(df_1$hasReddit, levels = c("No", "Yes"))
df_1$minInvestment <- lapply(df_1$minInvestment, convert_counts)
df_1$minInvestment <- factor(df_1$minInvestment, levels = c("No", "Yes"))

summary(df_1$rating)
str(df_1)
df_1$rating <- cut(df_1$rating, breaks = c(0, 2.600, 3.100, 3.70, 5.0), 
                   labels = c("low", "moderate-low", "moderate-high", "high"))
df_1$rating <- factor(df_1$rating, levels = c("low", "moderate-low", 
                                              "moderate-high", "high"))

summary(df_1$teamSize)
df_1$teamSize <- cut(df_1$teamSize, breaks = c(0, 7.00, 11.00, 17.0, 100), 
                     labels = c("low", "moderate-low", "moderate-high", "high"))
df_1$teamSize <- factor(df_1$teamSize, levels = c("low", "moderate-low", 
                                                  "moderate-high", "high"))

summary(df_1$priceUSD)
df_1$priceUSD <- cut(df_1$priceUSD, 
                     breaks = c(0, 0.05, 0.15, 0.56, 40000), 
                     labels = c("low", "moderate-low", "moderate-high", "high"))
df_1$priceUSD <- factor(df_1$priceUSD, levels = c("low", "moderate-low", 
                                              "moderate-high", "high"))

summary(df_1$coinNum)
df_1$coinNum <- cut(df_1$coinNum, breaks = c(0, 45000000, 150360000, 500000000, 
                                             200000000000000000), 
                    labels = c("low", "moderate-low", "moderate-high", "high"))
df_1$coinNum <- factor(df_1$coinNum, levels = c("low", "moderate-low", 
                                              "moderate-high", "high"))

summary(df_1$distributedPercentage)
df_1$distributedPercentage <- cut(df_1$distributedPercentage, 
                                  breaks = c(-0.2, 0.4000, 0.5600, 0.7, 2), 
                                  labels = c("low", "moderate-low", 
                                             "moderate-high", "high"))
df_1$distributedPercentage <- factor(df_1$distributedPercentage, levels = c("low", "moderate-low", 
  "moderate-high", "high"))

summary(df_1$ico_duration)
df_1$ico_duration <- cut(df_1$ico_duration, 
                         breaks = c(-0.2, 29.00, 46.00, 90.00, 780), 
                         labels = c("low", "moderate-low", "moderate-high", "high"))
df_1$ico_duration <- factor(df_1$ico_duration, levels = c("low", 
                                                          "moderate-low", 
                                                          "moderate-high", "high"))

# splitting data in train and test
trainIndex <- createDataPartition(1:nrow(df_1), p = 0.8, list = FALSE)
ico_naive_train <- df_final[trainIndex,]
ico_naive_test <- df_final[-trainIndex,]

# train naive bayes classifier
nb_model <- naiveBayes(success ~., data = ico_naive_train)

# prediction
nb_pred <- predict(nb_model, ico_naive_test)


# Evaluation
CrossTable(nb_pred, ico_naive_test$success,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

# Adding laplace smoothing for optimization ----
nb_model_optimized <- naiveBayes(success ~., data = ico_naive_train, laplace = 1)

nb_model_optimized_pred <- predict(nb_model_optimized, ico_naive_test)

CrossTable(nb_model_optimized_pred, ico_naive_test$success,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

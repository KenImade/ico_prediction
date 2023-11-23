# Data Processing ----

# set the seed for reproducibility ----
set.seed(123)

# libraries ---
library("mice")
library(tm)
library(SnowballC)
library(tidyr)
library(dplyr)
library(caret)
library(ggplot2)
library(ggcorrplot)

# import data ----
data <- read.csv("Data/data_cleaned.csv")

# structure of data ----
str(data)

# Loading data into dataframe ----
df <- data

# IDENTIFYING MISSING DATA ----
## replace blank or missing with NA ----
df[df == "" | df == " "] <- NA

summary(df)

## Count the number of missing values in each column ----
miss_count <- df %>% 
  summarise_all(~ sum(is.na(.))) %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "count") %>% 
  arrange(desc(count))

## Create a horizontal bar plot of missing values ----
ggplot(miss_count, aes(x = 0, xend = count, y = variable, yend = variable)) +
  geom_segment(size = 2, color = "skyblue") +
  geom_text(aes(x = count + 1, label = ifelse(count > 0, count, "")), hjust = 0) +
  labs(title = "Number of missing values in each variable", 
       x = "Count", y = "Variable") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, max(miss_count$count) + 5))

## Visualizing observations with missing values ----
# aggr(df, numbers = TRUE, prop = FALSE, combined = TRUE, cex.axis = 0.6)

## observations with missing values ----
missing_data <- df[rowSums(is.na(df)) > 0, ]
count(missing_data)

# HANDLING MISSING DATA ----

## Imputation of missing data ----
### SI method ----
#### priceUSD ----
si <- df
si$priceUSD[is.na(si$priceUSD)] <- mean(si$priceUSD, na.rm = TRUE)

#### teamSize ----
si$teamSize[is.na(si$teamSize)] <- round(mean(si$teamSize, na.rm = TRUE))

### MI method ----
colnames(df)
summary(df)
summary(df$teamSize) 
summary(df$priceUSD)
mi_df <- df %>%
  mutate("is_success" = ifelse(success == "Yes", 1, 0))

imi <- mice(subset(mi_df, select = c('hasVideo', 'rating', "teamSize", 'priceUSD', 
                                     "hasGithub","hasReddit", "coinNum", 
                                     "minInvestment", "distributedPercentage", 
                                     'is_success', 'ico_duration')), m = 5, 
            maxit = 50, method = "cart", seed = 500)

mi <- complete(imi)

colnames(mi)
colnames(df)

# Summary of teamSize
summary(si$teamSize)
summary(mi$teamSize) 

# Summary of priceUSD
summary(si$priceUSD)
summary(mi$priceUSD)

# Saving data from the two imputation methods
df_si <- si
df_mi <- cbind(df[!names(df) %in% names(mi)], mi)

summary(df_mi)

# Histograms of teamSize
df %>%
  ggplot(aes(x = teamSize)) +
  geom_histogram(binwidth = 3) +
  labs(title = "Distribution of teamSize before imputation",
       subtitle = "binwidth = 3")

df_si %>%
  ggplot(aes(x = teamSize)) +
  geom_histogram(binwidth = 3) +
  labs(title = "Distribution of teamSize after simple imputation method",
       subtitle = "binwidth = 3")

df_mi %>%
  ggplot(aes(x = teamSize)) +
  geom_histogram(binwidth = 3) +
  labs(title = "Distribution of teamSize after multiple imputation method",
       subtitle = "binwidth = 3")

# Boxplot of priceUSD
df %>%
  ggplot(aes(x = 1.0, y = log(priceUSD))) +
  geom_boxplot() +
  labs(title = "Distribution of priceUSD before imputation") +
  xlab("")

df_si %>%
  ggplot(aes(x = 1.0, y = log(priceUSD))) +
  geom_boxplot() +
  labs(title = "Distribution of priceUSD after simple imputation method") +
  xlab("")

df_mi %>%
  ggplot(aes(x = 1.0, y = log(priceUSD))) +
  geom_boxplot() +
  labs(title = "Distribution of priceUSD after multiple imputation method") +
  xlab("")


summary(df_mi$priceUSD)

### replace missing country with Unknown ----
df_2 <- df_mi %>%
  mutate(countryRegion = ifelse(is.na(countryRegion),"Unknown", countryRegion))

### replace blank cells with Unknown ----
df_2 <- df_2 %>%
  mutate(platform = ifelse(is.na(platform), "Unknown", platform))

# HANDLING OUTLIERS ----
summary(df_2)
boxplot(df_2$ico_duration, main = "Boxplot of ICO duration", 
        ylab = "ICO duration")
  

z_score <- scale(df_2$ico_duration)

# identify the data points with Z-score greater than 3 or less than -3 ----
outliers <- which(abs(z_score) > 3)
length(outliers)

df_outliers <- df_2[outliers, ]

# changing start date of outlier 
df_2[1092, "startDate"] <- "2020-01-10"

# calculating new ico_duration
df_2[1092, "ico_duration"] <- 
  as.numeric(as.Date.character(df_2[1092, "endDate"]) - 
               as.Date.character(df_2[1092, "startDate"]))

# exploring the dependent variable ----
table(df_2$success)
summary(df_2$success)

# Factoring target variable ----
df_2$success <- factor(df_2$success, levels = c("Yes", "No"))

# Dropping irrelevant columns ----
df_2 <- df_2 %>%
  select(-startDate, -endDate, -is_success)

summary(df_2)

# separate categorical, text, and numerical columns
df_numerical <- df_2 %>%
  select(-brandSlogan, -countryRegion, -platform, -success)

df_categorical <- df_2 %>%
  select(countryRegion, platform)

df_text <- df_2 %>%
  select(brandSlogan)

df_target <- df_2 %>%
  select(success)

# correlation plot of numerical variables
cor(df_numerical)
# corrplot(cor(df_numerical, method = "number") 
ggcorrplot(cor(df_numerical)) +
  labs(title = "Correlation Plot of Numerical Variables") +
  theme(plot.title = element_text(hjust = 0, size = 10, face = "bold"))

# Dummy coding categorical variables
df_categorical_encoded <- as.data.frame(model.matrix(~.-1, df_categorical))

# testing
sum(df_categorical_encoded[1, ])

# processing text column ----
corpus <- Corpus(VectorSource(df_text$brandSlogan))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)

dtm <- DocumentTermMatrix(corpus)

# Convert the dtm to a data frame and bind it with the response variable
df_text_tokenized <- as.data.frame(as.matrix(dtm))

# just cleaned data
df_cleaned <- cbind(df_target, df_numerical, df_categorical_encoded,
                    df_text_tokenized)

# save data
write.csv(df_cleaned, file = "Data/data_preprocessed.csv", row.names =  FALSE)
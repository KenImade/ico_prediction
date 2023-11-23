# DATA CLEANING ----
# Setting working directory
setwd(
"C:/Users/kenim/Desktop/School Materials/LUBS5990M - Machine Learning in Practice/Project/Code")

# display numbers in natural form
options(scipen = 999)

# load libraries
library("tidyverse")
library("dplyr")
library("VIM")
library(mice)
library(ggplot2)

# Loading data
data <- read.csv("Data/data.csv")

# Data structure
str(data)

# summary of dataset
summary(data)

# save in dataframe
df <- data

# IDENTIFY DUPLICATED VALUES ----
duplicated_row <- df[duplicated(df),]

# COLUMN CLEANING ----
## ID COLUMN ----
### dropping ID column ----
df <- df %>%
  select(-ID)

## Success column ----
unique(df$success)

### recoding target feature (success) as factor ----
df$success <- factor(df$success, levels = c("Y", "N"), labels = c("Yes", "No"))
                     
## brandSlogan ----
### converting to lower text ----
df <- df %>%
  mutate(brandSlogan = tolower(brandSlogan))

### strip irrelevant spaces ---
df <- df %>%
  mutate(brandSlogan = trimws(brandSlogan))

## HASVIDEO----
unique(df$hasVideo)
summary(df$hasVideo)
class(df$hasVideo)

## RATING ----
summary(df$rating)

## PRICEUSD ----
summary(df$priceUSD)

### filter out prices that are equal to 0 ----
df <- df %>%
  filter(priceUSD > 0 | is.na(priceUSD))

## COUNTRYREGION ----
summary(df$countryRegion)
str(df$countryRegion)
### change country names to lower case ----
df <- df %>%
  mutate(countryRegion = tolower(countryRegion))

### strip irrelevant spaces ---
df <- df %>%
  mutate(countryRegion = trimws(countryRegion))

summary(df$countryRegion)

### check for unique values ----
sort(unique(df$countryRegion))

### change "curaçao" to curacao ----
df <- df %>%
  mutate(countryRegion = ifelse(countryRegion == 'curaçao',
                                'curacao', countryRegion))

### testing ----
df %>%
  filter(countryRegion == 'curaçao') %>%
  count()

### change méxico to mexico ----
df <- df %>%
  mutate(countryRegion = ifelse(countryRegion == 'méxico', 'mexico',
                                countryRegion))

## START AND END DATE COLUMNS ----
### convert startDate and endDate columns to date type ----
df$startDate <- as.Date(df$startDate, format = "%d/%m/%Y")
df$endDate <- as.Date(df$endDate, format = "%d/%m/%Y")

### calculate duration of ICO campaign using startDate and endDate ----
df <- df %>%
  mutate('ico_duration' = endDate - startDate)

### convert ico_duration to numeric ----
df$ico_duration <- as.numeric(df$ico_duration)

### testing ----
class(df$ico_duration)

### filter out observations with negative ico_duration days ----
df <- df %>%
  filter(ico_duration >= 0)

## TEAMSIZE ----
summary(df$teamSize)

## GITHUB ----
summary(df$hasGithub)

## REDDIT ----
summary(df$hasReddit)

## PLATFORM ----
### Identify unique values ---
sort(unique(df$platform))

### change text to lower
df <- df %>%
  mutate(platform = tolower(platform))

### remove unnecessary space ----
df <- df %>%
  mutate(platform = trimws(platform))

### convert btc to bitcoin
df <- df %>%
  mutate(platform = ifelse(platform == 'btc', "bitcoin", platform))

### convert eth, ethererum, etherum, to ethereum
df <- df %>%
  mutate(platform = ifelse(platform == 'eth' | platform == 'ethererum' | 
                           platform == 'etherum', "ethereum", platform))

# convert x11 blockchain to x11
df <- df %>%
  mutate(platform = ifelse(platform == 'x11 blockchain', 'x11', platform))

# convert stellar protocol to stellar
df <- df %>%
  mutate(platform = ifelse(platform == 'stellar protocol', 'stellar', platform))

# convert "pos + pow", "pos,pow", "pow/pos" to "pos_pow"
df <- df %>%
  mutate(platform = ifelse(platform == "pos + pow" | platform == "pos,pow" |
                             platform == "pos/pow" | platform == "pow/pos", 
                           "pos_pow", platform))

## COINNUM ----
summary(df$coinNum)

## MININVESTMENT ----
summary(df$minInvestment)

## DISTRIBUTEDPERCENTAGE ----
summary(df$distributedPercentage)

### filter out values that are greater than 1 ----
df <- df %>%
  filter(!distributedPercentage > 1)

summary(df)

# Saving cleaned data to csv file ----
write.csv(df, file = "Data/data_cleaned.csv", row.names = FALSE)

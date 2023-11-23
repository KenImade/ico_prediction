# DATA EXPLORATION ----

# Load required libraries ----
library(ggplot2)
library(corrplot)
library(dplyr)
library(wordcloud)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wesanderson)

# display numbers in natural form
options(scipen = 999)

# Load cleaned data ----
df <- read.csv("Data/data.csv")


# Shape and structure of the data ----
str(df)
summary(df)

# Factor success variable
df$success <- factor(df$success, levels = c("Y", "N"), labels = c("Yes", "No"))

summary(df$success)

# hasVideo
df$hasVideo <- factor(df$hasVideo)
summary(df$hasVideo)
df %>% ggplot(aes(x = hasVideo, fill = success)) +
  geom_bar() +
  labs(title = "Distribution of hasVideo variable") +
  scale_x_discrete(labels = c("No", "Yes")) +
  scale_fill_discrete(labels = c("No", "Yes"))

# hasGithub
df$hasGithub <- factor(df$hasGithub)
summary(df$hasGithub)
df %>% ggplot(aes(x = hasGithub, fill = success)) +
  geom_bar() +
  labs(title = "Distribution of hasGithub variable") +
  scale_x_discrete(labels = c("No", "Yes")) +
  scale_fill_discrete(labels = c("No", "Yes"))

# hasReddit
df$hasReddit <- factor(df$hasReddit)
summary(df$hasReddit)
df %>% ggplot(aes(x = hasReddit, fill = success)) +
  geom_bar() +
  labs(title = "Distribution of hasReddit variable") +
  scale_x_discrete(labels = c("No", "Yes")) +
  scale_fill_discrete(labels = c("No", "Yes"))

# rating
df %>%
  filter(success == "No") %>%
  summarize(avg = mean(rating))

df %>% ggplot(aes(x = 1.0, y = rating, fill = success)) +
  geom_boxplot() +
  labs(title = "Distribution of rating variable") +
  facet_wrap(~ success)

# priceUSD
df %>%
  ggplot(aes(x = 1.0, y = log(priceUSD, 10), fill = success)) +
  geom_boxplot() +
  labs(title = "Distribution of log value of priceUSD variable") +
  facet_wrap(~ success) +
  ylab("log of priceUSD")

# countryRegion
df$countryRegion <- factor(df$countryRegion)
summary(df$countryRegion)

df_countries_five <- df %>%
  group_by(countryRegion) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(5) %>%
  as.data.frame()

df_countries_five %>%
  filter(countryRegion != "") %>%
  ggplot(aes(x = reorder(countryRegion, -count), y = count)) +
  geom_bar(stat = "identity") +
  ggtitle("Top 5 countries for ICO campaigns") +
  xlab("Country") +
  ylab("Number of ICO campaigns") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# Number of coins
df %>%
  ggplot(aes(x = 1.0, y = log(coinNum, 10), fill = success)) +
  geom_boxplot() +
  ggtitle("Distribution of log value of coinNum variable") +
  xlab("") +
  ylab("Log value of coinNum")
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) 

# Team Size
df %>%
  ggplot(aes(x = 1.0, y = teamSize, fill = success)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Distribution of team size") +
  xlab("") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())


# Minimum investment
df %>%
  ggplot(aes(x = 1.0, y =length(minInvestment), fill = success)) +
  geom_col(position = "fill") +
  facet_wrap(~ minInvestment) +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Distribution of minInvestment variable",
          subtitle = "0 = No minimum investment, 1 = Has minimum investment") +
  ylab("Proportion") +
  xlab("Minimum Investment")
  
# Distributed Percentage
df %>%
  ggplot(aes(x = 1.0, y = log(distributedPercentage, 10), fill = success)) +
  geom_boxplot() +
  labs(title = "Distribution of the log value of distributedPercentage variable") +
  ylab("Log of distributedPercentage")

# Common words used in ICO campaigns.
# Create a corpus from the text column in the dataframe
text <- c(df$brandSlogan)
class(text)
docs <- Corpus(VectorSource(text))

# Preprocessing the text data in the corpus
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

# Create a document-term matrix
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df_words <- data.frame(word = names(words),freq=words)

# Create a word cloud
set.seed(1234) # for reproducibility 
wordcloud(words = df_words$word, freq = df_words$freq, min.freq = 10,
          max.words=200, 
          random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

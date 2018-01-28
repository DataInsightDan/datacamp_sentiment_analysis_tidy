#' ---
#' title: Singing a Happy Song (or Sad?!)
#' author: "Mark Blackmore"  
#' date: "`r format(Sys.Date())`"  
#' output: 
#'   github_document:
#'     toc: true
#'     
#' ---

suppressWarnings(
  suppressPackageStartupMessages({
    library(tidyverse)
    library(tidytext)
  })
)

#' ### Tidying song lyrics  

load("./data/song_lyrics.rda")

# Pipe song_lyrics to the next line
tidy_lyrics <- song_lyrics %>% 
  # Transform the lyrics column to a word column
  unnest_tokens(word, lyrics)

# Print tidy_lyrics
tidy_lyrics  

#' ### Calculating total words per song

totals <- tidy_lyrics %>%
  # Count by song to find the word totals for each song
  count(song) %>%
  # Rename the new column
  rename(total_words = n)

# Print totals    
totals

lyric_counts <- tidy_lyrics %>%
  # Combine totals with tidy_lyrics using the "song" column
  left_join(totals, by = "song")

#' ### Sentiment analysis on song lyrics  

lyric_sentiment <- lyric_counts %>%
  # Implement sentiment analysis with the "nrc" lexicon
  inner_join(get_sentiments("nrc"))

lyric_sentiment %>%
  # Find how many sentiment words each song has
  count(song, sentiment , sort = TRUE)

#' ### The most positive and negative songs  

# What songs have the highest proportion of negative words?
lyric_sentiment %>%
  # Count using three arguments
  count(song, sentiment, total_words) %>%
  ungroup() %>%
  # Make a new percent column with mutate 
  mutate(percent = n / total_words) %>%
  # Filter for only negative words
  filter(sentiment == "negative") %>%
  # Arrange by descending percent
  arrange(desc(percent))

# What songs have the highest proportion of positive words?
lyric_sentiment %>%
  # Count using three arguments
  count(song, sentiment, total_words) %>%
  ungroup() %>%
  # Make a new percent column with mutate 
  mutate(percent = n / total_words) %>%
  # Filter for only negative words
  filter(sentiment == "positive") %>%
  # Arrange by descending percent
  arrange(desc(percent))

#' ### Sentiment and Billboard rank  

lyric_sentiment %>%
  filter(sentiment == "positive") %>%
  # Count by song, Billboard rank, and the total number of words
  count(song, rank, total_words) %>%
  ungroup() %>%
  # Use the correct dplyr verb to make two new columns
  mutate(percent = n / total_words,
         rank = 10 * floor(rank / 10)) %>%
  ggplot(aes(as.factor(rank), percent)) +
  # Make a boxplot
  geom_boxplot()

#' ### More on Billboard rank and sentiment scores  

lyric_sentiment %>%
  # Filter for only negative words
  filter(sentiment == "negative") %>%
  # Count by song, Billboard rank, and the total number of words
  count(song,rank, total_words)%>%
  ungroup() %>%
  # Mutate to make a percent column
  mutate(percent = n / total_words,
         rank = 10 * floor(rank / 10)) %>%
  # Use ggplot to set up a plot with rank and percent
  ggplot(aes(as.factor(rank), percent)) +
  # Make a boxplot
  geom_boxplot()

#' ### Sentiment scores by year

# How is negative sentiment changing over time?
lyric_sentiment %>%
  # Filter for only negative words
  filter(sentiment == "negative") %>%
  # Count by song, year, and the total number of words
  count(song, year, total_words) %>%
  ungroup() %>%
  mutate(percent = n / total_words,
         year = 10 * floor(year / 10)) %>%
  # Use ggplot to set up a plot with year and percent
  ggplot(aes(as.factor(year), percent)) +
  geom_boxplot()

# How is positive sentiment changing over time?
lyric_sentiment %>%
  filter(sentiment == "positive") %>%
  count(song, year, total_words) %>%
  ungroup() %>%
  mutate(percent = n / total_words,
         year = 10 * floor(year / 10)) %>%
  ggplot(aes(as.factor(year), percent)) +
  geom_boxplot()


#' ### Modeling negative sentiment  

negative_by_year <- lyric_sentiment %>%
  # Filter for negative words
  filter(sentiment == "negative") %>%
  count(song, year, total_words) %>%
  ungroup() %>%
  # Define a new column: percent
  mutate(percent = n / total_words)

# Specify the model with percent as the response and year as the predictor
model_negative <- lm(percent ~ year, data = negative_by_year)

# Use summary to see the results of the model fitting
summary(model_negative)

#' ### Modeling positive sentiment  

positive_by_year <- lyric_sentiment %>%
  filter(sentiment == "positive") %>%
  # Count by song, year, and total number of words
  count(song, year, total_words) %>%
  ungroup() %>%
  # Define a new column: percent
  mutate(percent = n / total_words)

# Fit a linear model with percent as the response and year as the predictor
model_positive <- lm(percent ~ year, data = positive_by_year)

# Use summary to see the results of the model fitting
summary(model_positive)

#' -------------
#'
#' ## Session info
#+ show-sessionInfo
sessionInfo()













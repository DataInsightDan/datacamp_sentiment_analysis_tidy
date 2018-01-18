#' ---
#' title: Analyzing TV News
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

#' ### Tidying TV news  

load("./data/climate_text.rda")

climate_text

# Pipe the climate_text dataset to the next line
tidy_tv <- climate_text %>%
  # Transform the non-tidy text data to tidy text data
  unnest_tokens(word, text)

tidy_tv

#' ### Counting totals

tidy_tv %>% 
  anti_join(stop_words) %>%
  # Count by word with sort = TRUE
  count(word, sort = TRUE)

tidy_tv %>%
  # Count by station
  count(station) %>%
  # Rename the new column station_total
  rename(station_total = n)

#' ### Sentiment analysis of tv news

tv_sentiment <- tidy_tv %>% 
  # Group by station
  group_by(station) %>% 
  # Define a new column station_total
  mutate(station_total = n()) %>%
  ungroup() %>%
  # Implement sentiment analysis with the NRC lexicon
  inner_join(get_sentiments("nrc"))

tv_sentiment

#' ### Which station uses the most positive or negative words?  

# Which stations use the most negative words?
tv_sentiment %>% 
  count(station, sentiment, station_total) %>%
  # Define a new column percent
  mutate(percent = n / station_total) %>%
  # Filter only for negative words
  filter(sentiment == "negative") %>%
  # Arrange by percent
  arrange(percent)

# Now do the same but for positive words
tv_sentiment %>% 
  count(station, sentiment, station_total) %>%
  mutate(percent = n / station_total) %>%
  filter(sentiment == "positive") %>%
  arrange(percent)












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

#' ### Which words contribute to the sentiment scores?  

tv_sentiment %>%
  # Count by word and sentiment
  count(word, sentiment) %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 words for each sentiment
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  # Set up the plot with aes()
  ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ sentiment, scales = "free") +
    coord_flip()

#' ### Word choice and TV station  

tv_sentiment %>%
  # Filter for only negative words
  filter(sentiment == "negative") %>%
  # Count by word and station
  count(word, station) %>%
  # Group by station
  group_by(station) %>%
  # Take the top 10 words for each station
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(paste(word, station, sep = "__"), n)) %>%
  # Set up the plot with aes()
  ggplot(aes(word, n, fill = station)) +
    geom_col(show.legend = FALSE) +
    scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
    facet_wrap(~ station, nrow = 2, scales = "free") +
    coord_flip()

#' ### Visualizing sentiment over time  

# Load the lubridate package
library(lubridate)

sentiment_by_time <- tidy_tv %>%
  # Define a new column using floor_date()
  mutate(date = floor_date(show_date, unit = "6 months")) %>%
  # Group by date
  group_by(date) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  # Implement sentiment analysis using the NRC lexicon
  inner_join(get_sentiments("nrc"))

sentiment_by_time %>%
  # Filter for positive and negative words
  filter(sentiment %in% c("positive", "negative")) %>%
  # Count by date, sentiment, and total_words
  count(date, sentiment, total_words) %>%
  ungroup() %>%
  mutate(percent = n / total_words) %>%
  # Set up the plot with aes()
  ggplot(aes(date, percent, fill = sentiment)) +
  geom_line(size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, lty = 2) +
  expand_limits(y = 0)


#' ### Word changes over time  

tidy_tv %>%
  # Define a new column that rounds each date to the nearest 1 month
  mutate(date = floor_date(show_date, unit = "1 month")) %>%
  filter(word %in% c("threat", "hoax", "denier",
                     "real", "warming", "hurricane")) %>%
  # Count by date and word
  count(date, word) %>%
  ungroup() %>%
  # Set up your plot with aes()
  ggplot(aes(date, n, color = word)) +
  # Make facets by word
  facet_wrap(~word) +
  geom_line(size = 1.5, show.legend = FALSE) +
  expand_limits(y = 0)


#' -------------
#'
#' ## Session info
#+ show-sessionInfo
sessionInfo()
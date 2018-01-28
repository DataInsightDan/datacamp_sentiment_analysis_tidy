#' ---
#' title: Tweets across the United States  
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

#' ### Sentiment lexicons

# Choose the bing lexicon
get_sentiments("bing") 

get_sentiments("bing") %>%
  count(sentiment) # Count words by sentiment

# Choose the nrc lexicon
get_sentiments("nrc") %>%
  count(sentiment) # Count words by sentiment

#' ### Implement an inner join  
 
load("./data/geocoded_tweets.rda")

# geocoded_tweets has been pre-defined
geocoded_tweets

# Access bing lexicon: bing
bing <- get_sentiments("bing")

# Use data frame with text data
geocoded_tweets %>%
  # With inner join, implement sentiment analysis using `bing`
  inner_join(bing)

#' ### What are the most common sadness words?  

# Access nrc lexicon: nrc
nrc <- get_sentiments("nrc")

# With inner join, implement sentiment analysis using `nrc`
tweets_nrc <- geocoded_tweets %>% 
  inner_join(nrc)

tweets_nrc

# Most common sadness words
tweets_nrc %>%
  # Filter to only choose the words associated with sadness
  filter(sentiment == "sadness") %>%
  # Group by word
  group_by(word) %>%
  # Use the summarize verb to find the mean frequency
  summarise(freq = mean(freq)) %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))


#' ### What are the most common joy words?

joy_words <- tweets_nrc %>%
  # Filter to choose only words associated with joy
  filter(sentiment == "joy") %>%
  # Group by each word
  group_by(word) %>%
  # Use the summarize verb to find the mean frequency
  summarise(freq = mean(freq)) %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))    

# Plot the Top 20 `joy_words`
joy_words %>%
  top_n(20) %>%
  mutate(word = reorder(word, freq)) %>%
  # Use aes() to put words on the x-axis and frequency on the y-axis
  ggplot(aes(word, freq)) +
  # Make a bar chart with geom_col()
  geom_col() + 
  coord_flip() 

#' ### Do people in different states use different words?  

tweets_nrc %>%
  # Find only the words for the state of Utah and associated with joy
  filter(state == "utah",
         sentiment == "joy") %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))

tweets_nrc %>%
  # Find only the words for the state of Louisiana and associated with joy
  filter(state == "louisiana",
         sentiment == "joy") %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))

#' ### Which states have the most positive Twitter users?  

# With inner join, implement sentiment analysis using `nrc`
tweets_bing <- geocoded_tweets %>% 
  inner_join(bing)

tweets_bing

tweets_bing %>% 
  # Group by two columns: state and sentiment
  group_by(state, sentiment) %>%
  # Use summarize to calculate the mean frequency for these groups
  summarise(freq = mean(freq)) %>%
  spread(sentiment, freq) %>%
  ungroup() %>%
  # Calculate the ratio of positive to negative words
  mutate(ratio = positive / negative,
         state = reorder(state, ratio)) %>%
  # Use aes() to put state on the x-axis and ratio on the y-axis
  ggplot(aes(state, ratio)) +
  # Make a plot with points using geom_point()
  geom_point() +
  coord_flip()


#' -------------
#'
#' ## Session info
#+ show-sessionInfo
sessionInfo()

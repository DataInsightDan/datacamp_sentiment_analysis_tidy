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
    library(dplyr)
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

# Access nrc lexicon: bing
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








#' -------------
#'
#' ## Session info
#+ show-sessionInfo
sessionInfo()
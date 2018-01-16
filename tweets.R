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
 
# load("./data/geocoded_tweets.rda")




#' -------------
#'
#' ## Session info
#+ show-sessionInfo
sessionInfo()
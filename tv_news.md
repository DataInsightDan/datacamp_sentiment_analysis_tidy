Analyzing TV News
================
Mark Blackmore
2018-01-17

-   [Tidying TV news](#tidying-tv-news)
-   [Counting totals](#counting-totals)
-   [Sentiment analysis of tv news](#sentiment-analysis-of-tv-news)
-   [Which station uses the most positive or negative words?](#which-station-uses-the-most-positive-or-negative-words)

``` r
suppressWarnings(
  suppressPackageStartupMessages({
    library(tidyverse)
    library(tidytext)
  })
)
```

### Tidying TV news

``` r
load("./data/climate_text.rda")

climate_text
```

    ## # A tibble: 593 x 4
    ##    station                                 show           show_date
    ##      <chr>                                <chr>              <dttm>
    ##  1   MSNBC                      Morning Meeting 2009-09-22 13:00:00
    ##  2   MSNBC                      Morning Meeting 2009-10-23 13:00:00
    ##  3     CNN                         CNN Newsroom 2009-12-03 20:00:00
    ##  4     CNN                     American Morning 2009-12-07 11:00:00
    ##  5   MSNBC                      Morning Meeting 2009-12-08 14:00:00
    ##  6   MSNBC       Countdown With Keith Olbermann 2009-12-10 06:00:00
    ##  7     CNN                      Sanjay Gupta MD 2009-12-12 12:30:00
    ##  8     CNN The Situation Room With Wolf Blitzer 2009-12-16 21:00:00
    ##  9   MSNBC       Countdown With Keith Olbermann 2009-12-19 01:00:00
    ## 10   MSNBC               The Rachel Maddow Show 2010-01-08 04:00:00
    ## # ... with 583 more rows, and 1 more variables: text <chr>

``` r
# Pipe the climate_text dataset to the next line
tidy_tv <- climate_text %>%
  # Transform the non-tidy text data to tidy text data
  unnest_tokens(word, text)

tidy_tv
```

    ## # A tibble: 41,076 x 4
    ##    station            show           show_date       word
    ##      <chr>           <chr>              <dttm>      <chr>
    ##  1   MSNBC Morning Meeting 2009-09-22 13:00:00        the
    ##  2   MSNBC Morning Meeting 2009-09-22 13:00:00   interior
    ##  3   MSNBC Morning Meeting 2009-09-22 13:00:00 positively
    ##  4   MSNBC Morning Meeting 2009-09-22 13:00:00      oozes
    ##  5   MSNBC Morning Meeting 2009-09-22 13:00:00      class
    ##  6   MSNBC Morning Meeting 2009-09-22 13:00:00      raves
    ##  7   MSNBC Morning Meeting 2009-09-22 13:00:00        car
    ##  8   MSNBC Morning Meeting 2009-09-22 13:00:00   magazine
    ##  9   MSNBC Morning Meeting 2009-09-22 13:00:00      slick
    ## 10   MSNBC Morning Meeting 2009-09-22 13:00:00        and
    ## # ... with 41,066 more rows

### Counting totals

``` r
tidy_tv %>% 
  anti_join(stop_words) %>%
  # Count by word with sort = TRUE
  count(word, sort = TRUE)
```

    ## Joining, by = "word"

    ## # A tibble: 3,699 x 2
    ##         word     n
    ##        <chr> <int>
    ##  1   climate  1627
    ##  2    change  1615
    ##  3    people   139
    ##  4      real   125
    ##  5 president   112
    ##  6    global   107
    ##  7     issue    87
    ##  8     trump    86
    ##  9   warming    85
    ## 10    issues    69
    ## # ... with 3,689 more rows

``` r
tidy_tv %>%
  # Count by station
  count(station) %>%
  # Rename the new column station_total
  rename(station_total = n)
```

    ## # A tibble: 3 x 2
    ##    station station_total
    ##      <chr>         <int>
    ## 1      CNN         10713
    ## 2 FOX News         10876
    ## 3    MSNBC         19487

### Sentiment analysis of tv news

``` r
tv_sentiment <- tidy_tv %>% 
  # Group by station
  group_by(station) %>% 
  # Define a new column station_total
  mutate(station_total = n()) %>%
  ungroup() %>%
  # Implement sentiment analysis with the NRC lexicon
  inner_join(get_sentiments("nrc"))
```

    ## Joining, by = "word"

``` r
tv_sentiment
```

    ## # A tibble: 10,175 x 6
    ##    station            show           show_date      word station_total
    ##      <chr>           <chr>              <dttm>     <chr>         <int>
    ##  1   MSNBC Morning Meeting 2009-09-22 13:00:00  interior         19487
    ##  2   MSNBC Morning Meeting 2009-09-22 13:00:00  interior         19487
    ##  3   MSNBC Morning Meeting 2009-09-22 13:00:00  interior         19487
    ##  4   MSNBC Morning Meeting 2009-09-22 13:00:00  sensuous         19487
    ##  5   MSNBC Morning Meeting 2009-09-22 13:00:00  sensuous         19487
    ##  6   MSNBC Morning Meeting 2009-09-22 13:00:00  striking         19487
    ##  7   MSNBC Morning Meeting 2009-09-22 13:00:00      join         19487
    ##  8   MSNBC Morning Meeting 2009-09-22 13:00:00 president         19487
    ##  9   MSNBC Morning Meeting 2009-09-22 13:00:00 president         19487
    ## 10   MSNBC Morning Meeting 2009-09-22 13:00:00    change         19487
    ## # ... with 10,165 more rows, and 1 more variables: sentiment <chr>

### Which station uses the most positive or negative words?

``` r
# Which stations use the most negative words?
tv_sentiment %>% 
  count(station, sentiment, station_total) %>%
  # Define a new column percent
  mutate(percent = n / station_total) %>%
  # Filter only for negative words
  filter(sentiment == "negative") %>%
  # Arrange by percent
  arrange(percent)
```

    ## # A tibble: 3 x 5
    ##    station sentiment station_total     n    percent
    ##      <chr>     <chr>         <int> <int>      <dbl>
    ## 1    MSNBC  negative         19487   526 0.02699235
    ## 2      CNN  negative         10713   331 0.03089704
    ## 3 FOX News  negative         10876   403 0.03705406

``` r
# Now do the same but for positive words
tv_sentiment %>% 
  count(station, sentiment, station_total) %>%
  mutate(percent = n / station_total) %>%
  filter(sentiment == "positive") %>%
  arrange(percent)
```

    ## # A tibble: 3 x 5
    ##    station sentiment station_total     n    percent
    ##      <chr>     <chr>         <int> <int>      <dbl>
    ## 1 FOX News  positive         10876   514 0.04726002
    ## 2      CNN  positive         10713   522 0.04872585
    ## 3    MSNBC  positive         19487   953 0.04890440

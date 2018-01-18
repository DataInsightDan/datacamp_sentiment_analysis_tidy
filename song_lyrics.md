Singing a Happy Song (or Sad?!)
================
Mark Blackmore
2018-01-18

-   [Tidying song lyrics](#tidying-song-lyrics)
-   [Calculating total words per song](#calculating-total-words-per-song)
-   [Sentiment analysis on song lyrics](#sentiment-analysis-on-song-lyrics)
-   [The most positive and negative songs](#the-most-positive-and-negative-songs)
-   [Sentiment and Billboard rank](#sentiment-and-billboard-rank)
-   [More on Billboard rank and sentiment scores](#more-on-billboard-rank-and-sentiment-scores)

``` r
suppressWarnings(
  suppressPackageStartupMessages({
    library(tidyverse)
    library(tidytext)
  })
)
```

### Tidying song lyrics

``` r
load("./data/song_lyrics.rda")

# Pipe song_lyrics to the next line
tidy_lyrics <- song_lyrics %>% 
  # Transform the lyrics column to a word column
  unnest_tokens(word, lyrics)

# Print tidy_lyrics
tidy_lyrics  
```

    ## # A tibble: 1,602,879 x 5
    ##     rank        song                        artist  year          word
    ##    <int>       <chr>                         <chr> <int>         <chr>
    ##  1     1 wooly bully sam the sham and the pharaohs  1965           sam
    ##  2     1 wooly bully sam the sham and the pharaohs  1965           the
    ##  3     1 wooly bully sam the sham and the pharaohs  1965          sham
    ##  4     1 wooly bully sam the sham and the pharaohs  1965 miscellaneous
    ##  5     1 wooly bully sam the sham and the pharaohs  1965         wooly
    ##  6     1 wooly bully sam the sham and the pharaohs  1965         bully
    ##  7     1 wooly bully sam the sham and the pharaohs  1965         wooly
    ##  8     1 wooly bully sam the sham and the pharaohs  1965         bully
    ##  9     1 wooly bully sam the sham and the pharaohs  1965           sam
    ## 10     1 wooly bully sam the sham and the pharaohs  1965           the
    ## # ... with 1,602,869 more rows

### Calculating total words per song

``` r
totals <- tidy_lyrics %>%
  # Count by song to find the word totals for each song
  count(song) %>%
  # Rename the new column
  rename(total_words = n)

# Print totals    
totals
```

    ## # A tibble: 4,341 x 2
    ##                      song total_words
    ##                     <chr>       <int>
    ##  1 0 to 100  the catch up         894
    ##  2     1 2 3 4 sumpin new         670
    ##  3        1 2 3 red light         145
    ##  4               1 2 step         437
    ##  5                1 thing         532
    ##  6          100 pure love         590
    ##  7              100 years         257
    ##  8                    123         220
    ##  9            18 and life         285
    ## 10            19 somethin         281
    ## # ... with 4,331 more rows

``` r
lyric_counts <- tidy_lyrics %>%
  # Combine totals with tidy_lyrics using the "song" column
  left_join(totals, by = "song")
```

### Sentiment analysis on song lyrics

``` r
lyric_sentiment <- lyric_counts %>%
  # Implement sentiment analysis with the "nrc" lexicon
  inner_join(get_sentiments("nrc"))
```

    ## Joining, by = "word"

``` r
lyric_sentiment %>%
  # Find how many sentiment words each song has
  count(song, sentiment , sort = TRUE)
```

    ## # A tibble: 39,564 x 3
    ##              song sentiment     n
    ##             <chr>     <chr> <int>
    ##  1           baby  positive   264
    ##  2           baby       joy   255
    ##  3      real love  positive   213
    ##  4          angel  positive   193
    ##  5      disturbia  negative   182
    ##  6 live your life  positive   174
    ##  7        my love  positive   173
    ##  8          angel       joy   164
    ##  9           damn  negative   164
    ## 10      disturbia   sadness   164
    ## # ... with 39,554 more rows

### The most positive and negative songs

``` r
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
```

    ## # A tibble: 4,169 x 5
    ##                              song sentiment total_words     n   percent
    ##                             <chr>     <chr>       <int> <int>     <dbl>
    ##  1                        bad boy  negative         237    77 0.3248945
    ##  2                      rack city  negative         458   142 0.3100437
    ##  3                ill tumble 4 ya  negative         269    79 0.2936803
    ##  4               time wont let me  negative         154    42 0.2727273
    ##  5 bang bang my baby shot me down  negative         163    40 0.2453988
    ##  6                     the stroke  negative         279    57 0.2043011
    ##  7                  the wild boys  negative         245    49 0.2000000
    ##  8                 pop that thang  negative         204    40 0.1960784
    ##  9                      disturbia  negative         956   182 0.1903766
    ## 10                        dance a  negative         407    72 0.1769042
    ## # ... with 4,159 more rows

``` r
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
```

    ## # A tibble: 4,295 x 5
    ##                                      song sentiment total_words     n
    ##                                     <chr>     <chr>       <int> <int>
    ##  1                  love to love you baby  positive         240    78
    ##  2 dance dance dance yowsah yowsah yowsah  positive         305    94
    ##  3                       i got the feelin  positive         141    35
    ##  4                           i love music  positive         252    61
    ##  5                     sweet and innocent  positive         218    51
    ##  6                    me and baby brother  positive         181    42
    ##  7                          love hangover  positive         173    40
    ##  8                     sweet cream ladies  positive         179    41
    ##  9                            mighty love  positive         482   110
    ## 10               keep feeling fascination  positive         189    43
    ## # ... with 4,285 more rows, and 1 more variables: percent <dbl>

### Sentiment and Billboard rank

``` r
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
```

![](song_lyrics_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-1.png)

### More on Billboard rank and sentiment scores

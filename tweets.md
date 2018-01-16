Tweets across the United States
================
Mark Blackmore
2018-01-16

-   [Sentiment lexicons](#sentiment-lexicons)
-   [Implement an inner join](#implement-an-inner-join)
-   [Session info](#session-info)

``` r
suppressWarnings(
  suppressPackageStartupMessages({
    library(dplyr)
    library(tidytext)
  })
)
```

### Sentiment lexicons

``` r
# Choose the bing lexicon
get_sentiments("bing") 
```

    ## # A tibble: 6,788 x 2
    ##           word sentiment
    ##          <chr>     <chr>
    ##  1     2-faced  negative
    ##  2     2-faces  negative
    ##  3          a+  positive
    ##  4    abnormal  negative
    ##  5     abolish  negative
    ##  6  abominable  negative
    ##  7  abominably  negative
    ##  8   abominate  negative
    ##  9 abomination  negative
    ## 10       abort  negative
    ## # ... with 6,778 more rows

``` r
get_sentiments("bing") %>%
  count(sentiment) # Count words by sentiment
```

    ## # A tibble: 2 x 2
    ##   sentiment     n
    ##       <chr> <int>
    ## 1  negative  4782
    ## 2  positive  2006

``` r
# Choose the nrc lexicon
get_sentiments("nrc") %>%
  count(sentiment) # Count words by sentiment
```

    ## # A tibble: 10 x 2
    ##       sentiment     n
    ##           <chr> <int>
    ##  1        anger  1247
    ##  2 anticipation   839
    ##  3      disgust  1058
    ##  4         fear  1476
    ##  5          joy   689
    ##  6     negative  3324
    ##  7     positive  2312
    ##  8      sadness  1191
    ##  9     surprise   534
    ## 10        trust  1231

### Implement an inner join

``` r
# load("./data/geocoded_tweets.rda")
```

------------------------------------------------------------------------

Session info
------------

``` r
sessionInfo()
```

    ## R version 3.4.2 (2017-09-28)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 16299)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.1252 
    ## [2] LC_CTYPE=English_United States.1252   
    ## [3] LC_MONETARY=English_United States.1252
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.1252    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] bindrcpp_0.2   tidytext_0.1.4 dplyr_0.7.4   
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_0.12.13      knitr_1.17        bindr_0.1        
    ##  [4] magrittr_1.5      mnormt_1.5-5      lattice_0.20-35  
    ##  [7] R6_2.2.2          rlang_0.1.2       plyr_1.8.4       
    ## [10] stringr_1.2.0     tools_3.4.2       parallel_3.4.2   
    ## [13] grid_3.4.2        nlme_3.1-131      broom_0.4.2      
    ## [16] psych_1.7.8       janeaustenr_0.1.5 htmltools_0.3.6  
    ## [19] yaml_2.1.14       assertthat_0.2.0  rprojroot_1.2    
    ## [22] digest_0.6.12     tibble_1.3.4      Matrix_1.2-11    
    ## [25] purrr_0.2.3       tidyr_0.7.1       reshape2_1.4.2   
    ## [28] SnowballC_0.5.1   tokenizers_0.1.4  glue_1.1.1       
    ## [31] evaluate_0.10.1   rmarkdown_1.6     stringi_1.1.5    
    ## [34] compiler_3.4.2    backports_1.1.1   foreign_0.8-69   
    ## [37] pkgconfig_2.0.1

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

#' ### 
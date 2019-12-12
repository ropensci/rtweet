
<!-- README.md is generated from README.Rmd. Please edit that file -->

## NOTE: Purpose of this Fork

This is a fork of rtweet that fixes the search_fullarchive function. The current version of rtweet maxes tweets to 100 when using this function although the Twitter API allows up to 500. Because of this, rtweet will burn 2-5x the API requests that it needs to, which costs money.

Feel free to use this until the pull request that has been submitted to rtweet has been merged.

## RTweet

R client for accessing Twitter’s REST and stream APIs. Check out the
[rtweet package documentation website](https://rtweet.info).


## Installation

To get the current released version from CRAN:

``` r
## install rtweet from CRAN
install.packages("rtweet")

## load rtweet package
library(rtweet)
```

To get the current development version from Github:

``` r
## install remotes package if it's not already
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

## install dev version of rtweet from github
remotes::install_github("kevintaylor/rtweet")

## load rtweet package
library(rtweet)
```


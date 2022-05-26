
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rtweet <img src="man/figures/logo.png" width="160px" align="right" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/ropensci/rtweet/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci/rtweet/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/rtweet)](https://cran.r-project.org/package=rtweet)
[![Coverage
Status](https://codecov.io/gh/ropensci/rtweet/branch/master/graph/badge.svg)](https://app.codecov.io/gh/ropensci/rtweet?branch=master)
![Downloads](https://cranlogs.r-pkg.org/badges/rtweet)
[![ZENODO](https://zenodo.org/badge/64161359.svg)](https://zenodo.org/badge/latestdoi/64161359)
[![rOpenSci](https://badges.ropensci.org/302_status.svg)](https://github.com/ropensci/software-review/issues/302)
[![JOSS](https://joss.theoj.org/papers/10.21105/joss.01829/status.svg)](https://doi.org/10.21105/joss.01829)
<!-- badges: end -->

Use twitter from R. Get started by reading `vignette("rtweet")`.

## Installation

To get the current released version from CRAN:

``` r
install.packages("rtweet")
```

You can install the development version of rtweet from GitHub with:

``` r
install.packages("rtweet", repos = 'https://ropensci.r-universe.dev')
```

## Usage

All users must be authenticated to interact with Twitter’s APIs. The
easiest way to authenticate is to use your personal twitter account -
this will happen automatically (via a browser popup) the first time you
use an rtweet function. See `auth_setup_default()` for details. Using
your personal account is fine for casual use, but if you are trying to
collect a lot of data it’s a good idea to authentication with your own
Twitter “app”. See `vignette("auth", package = "rtweet")` for details.

``` r
library(rtweet)
```

rtweet should be used in strict accordance with Twitter’s [developer
terms](https://developer.twitter.com/en/developer-terms/more-on-restricted-use-cases).

### Search tweets or users

Search for up to 1000 tweets containing \#rstats, the common hashtag
used to refer to the R language, excluding retweets:

``` r
rt <- search_tweets("#rstats", n = 1000, include_rts = FALSE)
```

Twitter rate limits cap the number of search results returned to 18,000
every 15 minutes. To request more than that, set
`retryonratelimit = TRUE` and rtweet will wait for rate limit resets for
you.

Search for 200 users with the \#rstats in their profile:

``` r
useRs <- search_users("#rstats", n = 200)
```

### Stream tweets

Randomly sample (approximately 1%) from the live stream of all tweets:

``` r
random_stream <- stream_tweets("")
```

Stream all geo-located tweets from London for 60 seconds:

``` r
stream_london <- stream_tweets(location = lookup_coords("london"), timeout = 60)
```

### Get friends and followers

Get all accounts followed by a user:

``` r
## get user IDs of accounts followed by R Foundation
R_Foundation_fds <- get_friends("_R_Foundation")

## lookup data on those accounts
R_Foundation_fds_data <- lookup_users(R_Foundation_fds$to_id)
```

Get all accounts following a user:

``` r
## get user IDs of accounts following R Foundation
R_Foundation_flw <- get_followers("_R_Foundation", n = 100)
R_Foundation_flw_data <- lookup_users(R_Foundation_flw$from_id)
```

If you want *all* followers, you’ll need you’ll need to set `n = Inf`
and `retryonratelimit = TRUE` but be warned that this might take a
*long* time.

### Get timelines

Get the most recent 200 tweets from R Foundation:

``` r
## get user IDs of accounts followed by R Foundation
tmls <- get_timeline("_R_Foundation", n = 100)
```

### Get favorites

Get the 10 most recently favorited statuses by R Foundation:

``` r
favs <- get_favorites("_R_Foundation", n = 10)
```

## Contact

Communicating with Twitter’s APIs relies on an internet connection,
which can sometimes be inconsistent.

If you have questions, or needs an example or want to share a [use
case](https://ropensci.org/usecases/), you can post them on [rOpenSci’s
discuss](https://discuss.ropensci.org/). Were you can [browse uses of
rtweet](https://discuss.ropensci.org/tags/c/usecases/10/rtweet) too.

With that said, if you encounter an obvious bug for which there is not
already an active [issue](https://github.com/ropensci/rtweet/issues),
please [create a new
issue](https://github.com/ropensci/rtweet/issues/new) with all code used
(preferably a reproducible example) on Github.

# Code of Conduct

Please note that this package is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/). By contributing to this
project, you agree to abide by its terms.

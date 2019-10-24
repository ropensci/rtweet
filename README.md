
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rtweet <img src="man/figures/logo.png" width="160px" align="right" />

[![Build
Status](https://travis-ci.org/ropensci/rtweet.svg?branch=master)](https://travis-ci.org/ropensci/rtweet)
[![CRAN
status](https://www.r-pkg.org/badges/version/rtweet)](https://cran.r-project.org/package=rtweet)
[![Coverage
Status](https://codecov.io/gh/ropensci/rtweet/branch/master/graph/badge.svg)](https://codecov.io/gh/ropensci/rtweet?branch=master)
[![DOI](https://zenodo.org/badge/64161359.svg)](https://zenodo.org/badge/latestdoi/64161359)
[![](https://badges.ropensci.org/302_status.svg)](https://github.com/ropensci/software-review/issues/302)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
![Downloads](https://cranlogs.r-pkg.org/badges/rtweet)
![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/rtweet)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.01829/status.svg)](https://doi.org/10.21105/joss.01829)

R client for accessing Twitter’s REST and stream APIs. Check out the
[rtweet package documentation website](https://rtweet.info).

### Package Functionality

There are several R packages for interacting with Twitter’s APIs. See
how {rtweet} compares to these others in the chart below.

| Task                        | [rtweet](https://github.com/ropensci/rtweet) | [twitteR](https://github.com/geoffjentry/twitteR) | [streamR](https://github.com/pablobarbera/streamR) | [RTwitterAPI](https://github.com/joyofdata/RTwitterAPI) |
| --------------------------- | -------------------------------------------- | ------------------------------------------------- | -------------------------------------------------- | ------------------------------------------------------- |
| Available on CRAN           | ✅                                            | ✅                                                 | ✅                                                  | ❌                                                       |
| Updated since 2016          | ✅                                            | ❌                                                 | ✅                                                  | ❌                                                       |
| Non-‘developer’ access      | ✅                                            | ❌                                                 | ❌                                                  | ❌                                                       |
| Extended tweets (280 chars) | ✅                                            | ❌                                                 | ✅                                                  | ❌                                                       |
| Parses JSON data            | ✅                                            | ✅                                                 | ✅                                                  | ❌                                                       |
| Converts to data frames     | ✅                                            | ✅                                                 | ✅                                                  | ❌                                                       |
| Automated pagination        | ✅                                            | ❌                                                 | ❌                                                  | ❌                                                       |
| Search tweets               | ✅                                            | ✅                                                 | ❌                                                  | ❓                                                       |
| Search users                | ✅                                            | ❌                                                 | ❌                                                  | ❓                                                       |
| Stream sample               | ✅                                            | ❌                                                 | ✅                                                  | ❌                                                       |
| Stream keywords             | ✅                                            | ❌                                                 | ✅                                                  | ❌                                                       |
| Stream users                | ✅                                            | ❌                                                 | ✅                                                  | ❌                                                       |
| Get friends                 | ✅                                            | ✅                                                 | ❌                                                  | ✅                                                       |
| Get timelines               | ✅                                            | ✅                                                 | ❌                                                  | ❓                                                       |
| Get mentions                | ✅                                            | ✅                                                 | ❌                                                  | ❓                                                       |
| Get favorites               | ✅                                            | ✅                                                 | ❌                                                  | ❓                                                       |
| Get trends                  | ✅                                            | ✅                                                 | ❌                                                  | ❓                                                       |
| Get list members            | ✅                                            | ❌                                                 | ❌                                                  | ❓                                                       |
| Get list memberships        | ✅                                            | ❌                                                 | ❌                                                  | ❓                                                       |
| Get list statuses           | ✅                                            | ❌                                                 | ❌                                                  | ❓                                                       |
| Get list subscribers        | ✅                                            | ❌                                                 | ❌                                                  | ❓                                                       |
| Get list subscriptions      | ✅                                            | ❌                                                 | ❌                                                  | ❓                                                       |
| Get list users              | ✅                                            | ❌                                                 | ❌                                                  | ❓                                                       |
| Lookup collections          | ✅                                            | ❌                                                 | ❌                                                  | ❓                                                       |
| Lookup friendships          | ✅                                            | ✅                                                 | ❌                                                  | ❓                                                       |
| Lookup statuses             | ✅                                            | ✅                                                 | ❌                                                  | ❓                                                       |
| Lookup users                | ✅                                            | ✅                                                 | ❌                                                  | ❓                                                       |
| Get retweeters              | ✅                                            | ✅                                                 | ❌                                                  | ❓                                                       |
| Get retweets                | ✅                                            | ✅                                                 | ❌                                                  | ❓                                                       |
| Post tweets                 | ✅                                            | ✅                                                 | ❌                                                  | ❌                                                       |
| Post favorite               | ✅                                            | ❌                                                 | ❌                                                  | ❌                                                       |
| Post follow                 | ✅                                            | ❌                                                 | ❌                                                  | ❌                                                       |
| Post messsage               | ✅                                            | ✅                                                 | ❌                                                  | ❌                                                       |
| Post mute                   | ✅                                            | ❌                                                 | ❌                                                  | ❌                                                       |
| Premium 30 day              | ✅                                            | ❌                                                 | ❌                                                  | ❌                                                       |
| Premium full archive        | ✅                                            | ❌                                                 | ❌                                                  | ❌                                                       |
| Run package tests           | ✅                                            | ❌                                                 | ❌                                                  | ❌                                                       |

## Responsible use

**{{rtweet}}** should be used in strict accordance with Twitter’s
[developer
terms](https://developer.twitter.com/en/developer-terms/more-on-restricted-use-cases).

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
remotes::install_github("ropensci/rtweet")

## load rtweet package
library(rtweet)
```

## Usage

All you need is a **Twitter account** (user name and password) and you
can be up in running in minutes\!

Simply send a request to Twitter’s API (with a function like
`search_tweets()`, `get_timeline()`, `get_followers()`,
`get_favorites()`, etc.) during an interactive session of R, authorize
the embedded **`rstats2twitter`** app (approve the browser popup), and
your token will be created and saved/stored (for future sessions) for
you\!

### API authorization

All users must be authorized to interact with Twitter’s APIs. To become
authorized, simply use a function like `search_tweets()`,
`get_timeline()`, `get_followers()`, or `get_favorites()` in an
interactive session an authorize via web browser popup on behalf of your
Twitter account\!

**It is no longer necessary to obtain a developer account and create
your own Twitter application to use Twitter’s API.** You may still
choose to do this (gives you more stability and permissions; see the
table at the bottom of this section), but **{rtweet}** should work out
of the box assuming (a) you are working in an interactive/live session
of R and (b) you have installed the
[**{httpuv}**](https://github.com/rstudio/httpuv) package.

  - If you still want to apply for a developer account and create your
    own application, see the `auth` vignette (or the API authorization
    section below) for additional instructions:
    <https://rtweet.info/articles/auth.html>.

| Task                       | rstats2twitter | user-app |
| -------------------------- | -------------- | -------- |
| Work interactively         | ✅              | ✅        |
| Search/lookup tweets/users | ✅              | ✅        |
| Get friends/followers      | ✅              | ✅        |
| Get timelines/favorites    | ✅              | ✅        |
| Get lists/collections      | ✅              | ✅        |
| Post tweets                | ❌              | ✅        |
| Run package tests          | ❌              | ✅        |
| Use Bearer token           | ❌              | ✅        |
| Read/Write Direct Messages | ❌              | ✅        |

### Vignettes

[Obtaining and using Twitter API
tokens](https://rtweet.info/articles/auth.html)

``` r
## quick overview of rtweet functions
vignette("auth", package = "rtweet")
```

[Quick overview of rtweet
package](https://rtweet.info/articles/intro.html)

``` r
## quick overview of rtweet functions
vignette("intro", package = "rtweet")
```

[Live streaming tweets data](https://rtweet.info/articles/stream.html)

``` r
## working with the stream
vignette("stream", package = "rtweet")
```

[Troubleshooting common rtweet
problems](https://rtweet.info/articles/FAQ.html)

``` r
## working with the stream
vignette("FAQ", package = "rtweet")
```

### Package features

#### Search tweets

Search for up to 18,000 (non-retweeted) tweets containing the rstats
hashtag.

``` r
## search for 18000 tweets using the rstats hashtag
rt <- search_tweets(
  "#rstats", n = 18000, include_rts = FALSE
)
```

Quickly visualize frequency of tweets over time using `ts_plot()`.

``` r
## plot time series of tweets
rt %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #rstats Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )
```

![](tools/readme/example-rstatsts.png)

Twitter rate limits cap the number of search results returned to 18,000
every 15 minutes. To request more than that, simply set
`retryonratelimit = TRUE` and rtweet will wait for rate limit resets for
you.

``` r
## search for 250,000 tweets containing the word data
rt <- search_tweets(
  "data", n = 250000, retryonratelimit = TRUE
)
```

Search by geo-location—for example, find 10,000 tweets in the English
language sent from the United States. *Note: `lookup_coords()` requires
users have [a Google API
key](https://developers.google.com/maps/documentation/javascript/tutorial)*

``` r
## search for 10,000 tweets sent from the US
rt <- search_tweets(
  "lang:en", geocode = lookup_coords("usa"), n = 10000
)

## create lat/lng variables using all available tweet and profile geo-location data
rt <- lat_lng(rt)

## plot state boundaries
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)

## plot lat and lng points onto state map
with(rt, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))
```

![](tools/readme/example-statemap.png)

#### Stream tweets

Randomly sample (approximately 1%) from the live stream of all tweets.

``` r
## random sample for 30 seconds (default)
rt <- stream_tweets("")
```

Stream all geo enabled tweets from London for 60 seconds.

``` r
## stream tweets from london for 60 seconds
rt <- stream_tweets(lookup_coords("london, uk"), timeout = 60)
```

Stream all tweets mentioning realDonaldTrump or Trump for a week.

``` r
## stream london tweets for a week (60 secs x 60 mins * 24 hours *  7 days)
stream_tweets(
  "realdonaldtrump,trump",
  timeout = 60 * 60 * 24 * 7,
  file_name = "tweetsabouttrump.json",
  parse = FALSE
)

## read in the data as a tidy tbl data frame
djt <- parse_stream("tweetsabouttrump.json")
```

#### Get friends

Retrieve a list of all the accounts a **user follows**.

``` r
## get user IDs of accounts followed by CNN
cnn_fds <- get_friends("cnn")

## lookup data on those accounts
cnn_fds_data <- lookup_users(cnn_fds$user_id)
```

##### Get followers

Retrieve a list of the **accounts following** a user.

``` r
## get user IDs of accounts following CNN
cnn_flw <- get_followers("cnn", n = 75000)

## lookup data on those accounts
cnn_flw_data <- lookup_users(cnn_flw$user_id)
```

Or if you really want ALL of their followers:

``` r
## how many total follows does cnn have?
cnn <- lookup_users("cnn")

## get them all (this would take a little over 5 days)
cnn_flw <- get_followers(
  "cnn", n = cnn$followers_count, retryonratelimit = TRUE
)
```

#### Get timelines

Get the most recent 3,200 tweets from cnn, BBCWorld, and foxnews.

``` r
## get user IDs of accounts followed by CNN
tmls <- get_timelines(c("cnn", "BBCWorld", "foxnews"), n = 3200)

## plot the frequency of tweets for each user over time
tmls %>%
  dplyr::filter(created_at > "2017-10-29") %>%
  dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by news organization",
    subtitle = "Twitter status (tweet) counts aggregated by day from October/November 2017",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )
```

![](tools/readme/example-tmls.png)

#### Get favorites

Get the 3,000 most recently favorited statuses by JK Rowling.

``` r
jkr <- get_favorites("jk_rowling", n = 3000)
```

#### Search users

Search for 1,000 users with the rstats hashtag in their profile bios.

``` r
## search for users with #rstats in their profiles
usrs <- search_users("#rstats", n = 1000)
```

#### Get trends

Discover what’s currently trending in San Francisco.

``` r
sf <- get_trends("san francisco")
```

#### Post actions

  - Posting (tweeting from R console) or reading direct messages require
    additional permissions
  - If you’d like to post Twitter statuses, follow or unfollow accounts,
    and/or read your direct messages, you’ll need to create your own
    Twitter app
  - To create your own Twitter app, follow the instructions in the
    authorization vignette on [obtaining and using access
    tokens](https://rtweet.info/articles/auth.html)

## Contact

Communicating with Twitter’s APIs relies on an internet connection,
which can sometimes be inconsistent. With that said, if you encounter an
obvious bug for which there is not already an active
[issue](https://github.com/ropensci/rtweet/issues), please [create a new
issue](https://github.com/ropensci/rtweet/issues/new) with all code used
(preferably a reproducible example) on Github.

[![ropensci\_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)

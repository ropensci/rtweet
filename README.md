
<!-- README.md is generated from README.Rmd. Please edit that file -->
rtweet <img src="man/figures/logo.png" align="right" />
=======================================================

[![Build Status](https://travis-ci.org/mkearney/rtweet.svg?branch=master)](https://travis-ci.org/mkearney/rtweet) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/rtweet)](https://CRAN.R-project.org/package=rtweet) ![Downloads](https://cranlogs.r-pkg.org/badges/rtweet) ![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/rtweet) [![Travis-CI Build Status](https://travis-ci.org/mkearney/rtweet.svg?branch=master)](https://travis-ci.org/mkearney/rtweet) [![Coverage Status](https://img.shields.io/codecov/c/github/mkearney/dplyr/master.svg)](https://codecov.io/gh/mkearney/rtweet) [![Rdoc](http://www.rdocumentation.org/badges/version/rtweet)](http://www.rdocumentation.org/packages/rtweet)

R client for interacting with Twitter's REST and stream API's.

Check out the [rtweet package documentation website](http://rtweet.info).

Install
-------

To get the current released version from CRAN:

``` r
install.packages("rtweet")
library(rtweet)
```

To get the current development version from Github:

``` r
if (!"devtools" %in% installed.packages()) {
  install.packages("devtools")
}
devtools::install_github("mkearney/rtweet")
library(rtweet)
```

Getting started
---------------

-   ***Quick authorization method***: To make your life easier, follow the recommended steps in [obtaining and using access tokens](http://rtweet.info/index.html). However, for a quick start (note: much slower in long term), you can also follow the instructions below or via the [rtweet documentation website](http://rtweet.info/index.html).

Update: rtweet's website has a new domain: <http://rtweet.info>

-   First, you'll need to [create a Twitter app](https://apps.twitter.com/). For the callback field, make sure to enter: `http://127.0.0.1:1410`.

-   Once you've created an app, record your consumer (api) and secret keys. Screeshots can be found [here](http://rtweet.info/articles/auth.html).

-   Generate a token by using the `create_token()` function.

``` r
## name assigned to created app
appname <- "rtweet_token"
## api key (example below is not a real key)
key <- "XYznzPFOFZR2a39FwWKN1Jp41"
## api secret (example below is not a real key)
secret <- "CtkGEWmSevZqJuKl6HHrBxbCybxI1xGLqrD5ynPd9jG0SoHZbD"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)
```

-   Once `twitter_token` is part of your global environment, rtweet functions should find it. However, using this method, the token will not automatically load in future sessions (you'll need to create a token every time you start a new session).

-   Although not necessary, functions also accept tokens via the `token` argument. For example:

``` r
rt <- search_tweets("data science", n = 1000, token = twitter_token)
```

-   ***Recommended authorization method***: [Obtaining and using access tokens](http://rtweet.info/articles/auth.html) (vignette showing how to *sustainably* setup authorization to use Twitter's APIs).

Vignettes
---------

-   [Obtaining and using user access tokens](http://rtweet.info/articles/auth.html)

``` r
## authorizing API access
vignette("auth", package = "rtweet")
```

-   [Quick overview of rtweet package](http://rtweet.info/articles/intro.html)

``` r
## quick overview of rtweet functions
vignette("intro", package = "rtweet")
```

-   [Live streaming tweets data](http://rtweet.info/articles/stream.html)

``` r
## working with the stream
vignette("stream", package = "rtweet")
```

Package description
-------------------

More technical description: An implementation of calls designed to extract and organize Twitter data via Twitter's REST and stream API's. Functions formulate GET and POST requests and convert response objects to more user friendly structures, e.g., data frames or lists. Specific consideration is given to functions designed to return tweets data from searches, streams, and timelines and user ids from friends and followers lists.

Contact
-------

Communicating with Twitter's APIs relies on an internet connection, which can sometimes be inconsistent. With that said, if you encounter an obvious bug for which there is not already an active [issue](https://github.com/mkearney/rtweet/issues), please [create a new issue](https://github.com/mkearney/rtweet/issues/new) with all code used (preferably a reproducible example) on Github.

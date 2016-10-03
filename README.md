<!-- README.md is generated from README.Rmd. Please edit that file -->
<img src="inst/rt_logo/rt.png" width="5%", alt=""> rtweet: Collecting Twitter Data
==================================================================================

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/rtweet)](https://CRAN.R-project.org/package=rtweet) ![Downloads](https://cranlogs.r-pkg.org/badges/rtweet) ![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/rtweet) [![Travis-CI Build Status](https://travis-ci.org/mkearney/rtweet.svg?branch=master)](https://travis-ci.org/mkearney/rtweet) [![codecov](https://codecov.io/gh/mkearney/rtweet/branch/master/graph/badge.svg)](https://codecov.io/gh/mkearney/rtweet)

R client for collecting data via Twitter's REST and stream API's.

Key features

-   ***NEW*** (dev version on Github): Out of the box functionality! Start using `rtweet` the moment you install the package. Limited authorization access provided for users looking to test-drive the package before [obtaining and using access tokens](https://github.com/mkearney/rtweet/blob/master/vignettes/tokens.Rmd).

-   Tweet from your R console using the `post_tweet()` function!

-   Stream a random sample of tweets using `stream_tweets()`. The function default, `q = ""`, now streams a random sample of all tweets.

-   Save as CSV: If you'd like to open Twitter data in Excel or SPSS, use the `save_as_csv` function.

-   Gather **tweet data** by searching past tweets `search_tweets()`, streaming live tweets `stream_tweets()`, collecting tweets from a user's timeline `get_timeline()`, or gathering all the tweets favorited by a user `get_favorites()`.

-   Gather **user data** by looking up Twitter users `lookup_users()`. Easily return data on thousands of users.

-   Gather **followers** and **friends** data by collecting the ids of accounts *following* a user `get_followers()` or the ids of accounts *followed by* a user `get_friends()`.

-   Organized and easily translatable data formats. Functions return tidy data frames **ready** for data analysis.

Install
-------

To get the current released version from CRAN:

``` r
install.packages("rtweet")
library(rtweet)
```

To get the current development version from github:

``` r
install.packages("devtools")
devtools::install_github("mkearney/rtweet")
```

Getting started
---------------

-   ***Quick authorization method***: To make your life easier, follow the recommended steps in [obtaining and using access tokens](https://github.com/mkearney/rtweet/blob/master/vignettes/tokens.Rmd). However, for a quick start (note: much slower in long term), you can also follow the instructions below.

-   First, you'll need to [create a Twitter app](https://apps.twitter.com/). For the callback field, make sure to enter: `http://127.0.0.1:1410`.

-   Once you've created an app, record your consumer (api) and secret keys.

-   Generate a token by using the `create_token()` function.

``` r
twitter_token <- create_token(app = "rtweet_tokens", # whatever you named app
  consumer_key = "XZgqotgOZNKlLFJqFbd8NjUtL",
  consumer_secret = "1rDnU3H3nrxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
# I xxx'd out the secret key, but you get the idea
```

-   Make sure to specify `twitter_token` every time you use a data retrieval function, like the example below:

``` r
tw <- search_tweets("r", n = 1200, token = twitter_token, lang = "en")
```

-   ***Recommended authorization method***: [Obtaining and using access tokens](https://github.com/mkearney/rtweet/blob/master/vignettes/tokens.Rmd) (vignette showing how to *sustainably* setup authorization to use Twitter's APIs).

Package description
-------------------

More technical description: An implementation of calls designed to extract and organize Twitter data via Twitter's REST and stream API's. Functions formulate GET and POST requests and convert response objects to more user friendly structures, e.g., data frames or lists. Specific consideration is given to functions designed to return tweets, friends, and followers.

Contact
-------

Email me at <mkearney@ku.edu>

To Do List
----------

**Data Analysis Helpers** - Network analysis matrices and edge lists data structures - Text cleaner/utility functions - Data base management (SQL) integration for big data

**API Functions**

-   `get_retweeters()` Retrieve users retweeting a status (in progress)
-   `get_list()` Retrieve users in list

**Vignettes**

-   Word cloud, textual analysis
-   Network analysis featuring `get_friends()` and `get_followers()`

**Documentation**

-   More examples, more details, and list return columns with descriptions
-   Search query syntax
-   Stream syntax i.e., filter vs tracking vs location
-   Geo-based and date-specific queries

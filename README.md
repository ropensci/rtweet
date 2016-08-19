<!-- README.md is generated from README.Rmd. Please edit that file -->
rtweet: Collecting Twitter Data
===============================

[![CRAN Version](http://www.r-pkg.org/badges/version/rtweet)](http://cran.r-project.org/package=rtweet) ![Downloads](http://cranlogs.r-pkg.org/badges/rtweet)

R package for collecting Twitter data via Twitter's REST and stream API's.

Key features

-   ***SAVING DATA AS CSV***: Several people have asked how to save data as a CSV file (or something they can open with Excel). There is now a `save_as_csv` function in the development (github) version of `rtweet`. If you'd like to save CSV files using the CRAN version, then [use this code](https://gist.github.com/mkearney/7474b64f9db177435de540f5fa63a087) until the next update is posted to CRAN.

-   ***UPDATE***: Most functions now return data tables for both tweets **and** users. So, if you search for tweets, you also get data for the users responsible for the returned tweets. If you look up users, you also get the most recent tweet for each user.

-   Gather **tweet data** by searching past tweets `search_tweets()`, streaming live tweets `stream_tweets()`, or collecting tweets from a user's timeline `get_timeline()`. Easily return data on **thousands** of tweets at a time.

-   Gather **user data** by looking up Twitter users `lookup_users()`. Easily return data on thousands of users.

-   Gather **followers** and **friends** data by collecting the ids of accounts *following* a user `get_followers()` or the ids of accounts *followed by* a user `get_friends()`.

-   Organized and easily translatable data formats. Functions return tibble data tables **ready** for data analysis.

-   Tweet data functions return not only text of tweets, but a host of other variables (up t 27 columns), including the number of times a tweet has been retweeted (`retweet_count`) and favorited (`favorite_count`). To gauge user interactions, there are also seperate variables that identify replies by source tweet (`in_reply_to_status_id_str`) and by source user ID (`in_reply_to_status_id_str`). Variables also indicate whether the tweet quotes another tweet (`is_quote_status`), and, if so, the quoted tweet ID (`quoted_status_id_str`).

Install
-------

To get the current released version from CRAN:

``` r
install.packages("rtweet")
library(rtweet)
```

To get the current development version from github:

``` r
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
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

**API Functions**

-   `get_retweeters()` Retrieve users retweeting a status (in progress)
-   `get_list()` Retrieve users in list
-   `lookup_tweets()` Look up tweets via status\_id.

**Vignettes**

-   Guide to `stream_tweets()`
-   Guide to `get_friends()` and `get_followers()`

**Documentation**

-   Search query syntax
-   Stream syntax i.e., filter vs tracking vs location

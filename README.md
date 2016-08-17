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

-   *Quick authorization method*: go (here)\[<http://apps.twitter.com/app/new>\] and create a Twitter app. In the callback field, enter: `http://127.0.0.1:1410`
-   Enter your consumer (api) and secret keys

``` r
token <- create_token(app = "rtweet_tokens", # whatever you named app
  consumer_key = "XZgqotgOZNKlLFJqFbd8NjUtL",
  consumer_secret = "1rDnU3H3nrxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
# I xxx'd out the secret key, but you get the idea
```

-   Specify token in call

``` r
tw <- search_tweets("r", n = 1200, token = token, lang = "en")
#> Searching for tweets...
#> Finished collecting tweets!
tw
#> # A tibble: 1,200 x 27
#>             created_at          status_id retweet_count favorite_count
#>                 <time>              <chr>         <int>          <int>
#> 1  2016-08-16 19:57:06 765714031621451777          1334           2215
#> 2  2016-08-16 17:26:22 765676100747833345          1711           7831
#> 3  2016-08-15 23:16:23 765401795258482689          1907           6685
#> 4  2016-08-16 20:57:47 765729303002685441             0              0
#> 5  2016-08-16 20:57:46 765729301702606848             0              0
#> 6  2016-08-16 20:57:46 765729301560000513          1466              0
#> 7  2016-08-16 20:57:46 765729300427464704             2              0
#> 8  2016-08-16 20:57:46 765729300255428608             0              0
#> 9  2016-08-16 20:57:46 765729299676758016             2              0
#> 10 2016-08-16 20:57:46 765729299374731265             0              0
#> # ... with 1,190 more rows, and 23 more variables: text <chr>,
#> #   in_reply_to_status_id <chr>, in_reply_to_user_id <chr>,
#> #   is_quote_status <lgl>, quoted_status_id <chr>, source <chr>,
#> #   lang <chr>, user_id <chr>, user_mentions <list>, hashtags <list>,
#> #   urls <list>, is_retweet <lgl>, retweet_status_id <chr>,
#> #   place_name <chr>, country <chr>, long1 <dbl>, long2 <dbl>,
#> #   long3 <dbl>, long4 <dbl>, lat1 <dbl>, lat2 <dbl>, lat3 <dbl>,
#> #   lat4 <dbl>
```

``` r
users_data(tw)
#> # A tibble: 1,200 x 19
#>       user_id                name     screen_name
#>         <chr>               <chr>           <chr>
#> 1   109303622 Pretty Little Liars     PLLTVSeries
#> 2   910874659             RiceGum         RiceGum
#> 3    75641903       Carrie Fisher   carrieffisher
#> 4   559943767        Blondes R Us     Blonds_R_US
#> 5  3324160841   CaramelApple Chey   cheyennelanee
#> 6   300487221         lil bitty ♊️    BureenuhDuhh
#> 7    30004264        gayleen gray      ggatguelph
#> 8   267558200    Reddit Beertrade redditbeertrade
#> 9    28910724          JoeBucsFan      JoeBucsFan
#> 10 3995336123      Sprint Forward   sprintforward
#> # ... with 1,190 more rows, and 16 more variables: location <chr>,
#> #   description <chr>, url <chr>, protected <lgl>, followers_count <int>,
#> #   friends_count <int>, listed_count <int>, created_at <time>,
#> #   favourites_count <int>, utc_offset <int>, time_zone <chr>,
#> #   geo_enabled <lgl>, verified <lgl>, statuses_count <int>, lang <chr>,
#> #   description_urls <list>
```

-   ***Recommended authorization method***: [Obtaining and using access tokens](https://github.com/mkearney/rtweet/blob/master/vignettes/tokens.md) (vignette showing how to *sustainably* setup authorization to use Twitter's APIs)

-   Function demo: [Collecting and analyzing tweets using the `search_tweets()` function](https://github.com/mkearney/rtweet/blob/master/vignettes/search_tweets.md) (vignette showing how to use the `search_tweets()` function)

Package description
-------------------

More technical description: An implementation of calls designed to extract and organize Twitter data via Twitter's REST and stream API's. Functions formulate GET and POST requests and convert response objects to more user friendly structures, e.g., data frames or lists. Specific consideration is given to functions designed to return tweets, friends, and followers.

Contact
-------

Email me at <mkearney@ku.edu>

To Do List
----------

-   API Functions
-   `get_retweeters()` Retrieve users retweeting a status (in progress)
-   `get_list()` Retrieve users in list
-   `search_users()` Search for users by name, interest, etc.
-   `lookup_tweets()` Look up tweets via status\_id.

-   Vignettes
-   Guide to `stream_tweets()`
-   Guide to `get_friends()` and `get_followers()`

-   Documentation
-   Search query syntax
-   Stream syntax i.e., filter vs tracking vs location

# rtweet: Collecting Twitter Data

[![CRAN Version](http://www.r-pkg.org/badges/version/rtweet)](http://cran.r-project.org/package=rtweet)
![Downloads](http://cranlogs.r-pkg.org/badges/rtweet)

R package for collecting Twitter data via Twitter's REST and stream API's.

Key features

- ***UPDATE***: Most functions now return data tables for both tweets
**and** users. So, if you search for tweets, you also get data
for the users responsible for the returned tweets. If you look up
users, you also get the most recent tweet for each user.

- Gather **tweet** data by searching past tweets `search_tweets()`, 
streaming live tweets `stream_tweets()`, or collecting tweets from 
a user's timeline `get_timeline()`. Easily return data on 
**thousands** of tweets at a time.

- Gather **user** data by looking up Twitter users `lookup_users()`.
Easily return data on thousands of users.

- Gather **followers** and **friends** data by collecting the ids of 
accounts *following* a user `get_followers()` or the ids of 
accounts *followed by* a user `get_friends()`.

- Organized and easily translatable data formats. Functions return
tibble data tables **ready** for data analysis.

- Tweet data functions return not only text of tweets, but a host of 
other variables (up t 27 columns), including the number of times 
a tweet has been retweeted (`retweet_count`) and 
favorited (`favorite_count`). To gauge user interactions, there are 
also seperate variables that identify replies by source tweet 
(`in_reply_to_status_id_str`) and by source user ID 
(`in_reply_to_status_id_str`). Variables also indicate whether the 
tweet quotes another tweet (`is_quote_status`), and, if so, the 
quoted tweet ID (`quoted_status_id_str`).

## Install
To get the current released version from CRAN:
```{r}
install.packages("rtweet")
library(rtweet)
```

To get the current development version from github:
```{r}
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
devtools::install_github("mkearney/rtweet")
```

## Getting started

- Obtaining and using access tokens (a guide to gaining 
authorization to use Twitter's APIs): https://github.com/mkearney/rtweet/blob/master/vignettes/tokens.md

- Collecting and analyzing tweets using the `search_tweets()` function: https://github.com/mkearney/rtweet/blob/master/vignettes/search_tweets.md

## Package description

More technical description: An implementation of calls designed to extract
and organize Twitter data via Twitter's REST and stream
API's. Functions formulate GET and POST requests and
convert response objects to more user friendly structures,
e.g., data frames or lists. Specific consideration is
given to functions designed to return tweets, friends,
and followers.

## Contact
Email me at mkearney@ku.edu


## To Do List

- API Functions
  - `get_retweeters()` Retrieve users retweeting a status (in progress)
  - `list_users()` Retrieve users in list
  - `search_users()` Search for users by name, interest, etc.
 
- Vignettes
  - Guide to `stream_tweets()`
  - Guide to `get_friends()` and `get_followers()`

- Documentation
  - Search query syntax
  - Stream syntax i.e., filter vs tracking vs location

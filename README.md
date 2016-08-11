# rtweet: Collecting Twitter Data

[![CRAN Version](http://www.r-pkg.org/badges/version/rtweet)](http://cran.r-project.org/package=rtweet)
![Downloads](http://cranlogs.r-pkg.org/badges/rtweet)

R package for collecting Twitter data via Twitter's REST and stream API's.

Key features

- Gather **tweet** data by searching `search_tweets()` past tweets or 
by streaming `stream_tweets()` live tweets. Easily return data on 
thousands of tweets at a time.

- Gather **user** data by looking up `lookup_users()` information
on specific Twitter users. Easily return data on thousands of users
at a time.

- Organized and easily translatable data formats. Functions return
data frames that are **ready** for data analysis the moment they
are received.

- Tweet data functions return not only text of tweets, but a host of 
other variables, including the number of times a tweet has been 
retweeted (`retweet_count`) and favorited (`favorite_count`).
To gauge user interactions, there are also seperate variables that
identify replies by source tweet (`in_reply_to_status_id_str`) 
and by source user ID (`in_reply_to_status_id_str`). Variables also
indicate whether the tweet quotes another tweet (`is_quote_status`), 
and, if so, the quoted tweet ID (`quoted_status_id_str`).

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
  - Add `source` variable to tweets object
 
- Vignettes
  - Guide to `stream_tweets()`
  - Guide to `get_friends()` and `get_followers()`
  - Data analysis demonstration (regression)
  - Text analysis demonstration (word cloud)

- Documentation
  - Search query syntax
  - Stream syntax i.e., filter vs tracking vs location
  - Return object (names) information
  - Class-based error checks (with messages)

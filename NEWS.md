# rtweet 0.2.0

* Since previous CRAN release, numerous new features and improvements
to functions returning tweets, user data, and ids.

* Search function now optimized to return more tweets per search.

* Numerous improvements to stability, error checks, and namespace
management.

# rtweet 0.1.91

* Improvements to `get_friends` and `get_followers`. Returns list 
with value (`next_cursor`) used for next page of results. When
this value is 0, all results have been returned.

* Functions `get_friends` and `get_followers` now return the list
of user ids as a tibble data table, which makes the print out much
cleaner.

# rtweet 0.1.9

* Improved scrolling methods such that `search_tweets` and 
`get_timeline` should return a lot more now

* Added `parser` function to return status (tweets) AND user (users)
data frames when available. As a result, the parsed output for some 
functions now comes as a list containing two data frames.

# rtweet 0.1.8

* Added `get_timeline` function that returns tweets from selected user

* Added vignettes covering tokens and search tweets

* Fixed issue with `count` argument in search and user functions

# rtweet 0.1.7

* Fixed parsing issue for return objects with omitted variables

* Added `clean_tweets` convenience function for text analysis

* More examples included in documentation.

# rtweet 0.1.6

* Added `recode_error` argument to `get_friends` function. This is
especially useful for tracking networks over time.

* Further integrated `ROAuth` methods/objects to increase 
compatibility with `twitteR` authorization procedures.

* Improved token checking procedures.

# rtweet 0.1.4

* Added `NEWS.md` file

* Added `key features` and more descriptions to `README.md`.

# rtweet 0.1.3

* There are now two stable parse (convert json obj to data frame)
types. For user objects (e.g., output of `lookup_users`), there
is `parse_user`. For tweet objects (e.g., output of `search_tweets`
or `stream_tweets`), there is `parse_tweets`. 

* New parse functions are now exported, so they should available 
for use with compatible Twitter packages or user-defined API 
request operations.

# rtweet 0.1.2

* More parsing improvements

* Added `format_date` function

* Various stability improvements

# rtweet 0.1.1

* Improvements to parse functions

# rtweet 0.1.0

* Initial release

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

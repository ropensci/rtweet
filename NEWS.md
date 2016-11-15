# rtweet 0.3.7
* Reworked `ts_plot` to enable different filtered time series and
an aesthetic overhaul of the plot function as well.

# rtweet 0.3.6
* Added `as_double` argument to provide flexibility in handling 
id variables (as_double provides performance boost but can create
problems when printing and saving, depending on format). By default
functions will return IDs as character vectors.
* Numerous improvements made to parsing and bug fixes to lookup
and search functions.
 
# rtweet 0.3.5
* `clean_tweets` argument provided to allow user more control over
encoding and handling of non-ascii characters.
* Fixed issue with `search_users` and implemented several 
improvements to `stream_tweets` and `plot_ts`.

# rtweet 0.3.4
* Implemented robust methods to fetch tokens (whether set as
environment variable, .httr-oauth file, or if the tokens exist
in the global environment). Functions now search for variations
in the labeling of tokens---i.e., if your token(s) are saved as
`twitter_tokens`, `twitter_token`, `tokens`, or `token`, rtweet
will find it.
* Fixed issues with parsing tweets and users data.
* Stability improvements to `search_tweets` and `stream_tweeets`

# rtweet 0.3.3
* Flattened recursive columns for more reliable parsing and various
speed enhancements

# rtweet 0.3.2
* Added built-in, encrypted tokens
* Fixed issues with tweets parsing and reading streams
* Numerous speed improvements

# rtweet 0.3.1
* `include_retweets` arg added to `search_tweets()` function.
* `user_id` class changed to double when parsed. double is significantly
faster and consumes less space. it's also capable of handling the length of
id scalars, so the only downside is truncated printing.

# rtweet 0.3.0
* New CRAN version!
* Lots of improvements to stability and entirely new functions to
play around with (see previous news updates for more info).
* Added more documentation all round, including help features, examples, and
vignette infrastructure.

# rtweet 0.2.92
* Added gzip option for `stream_tweets()`

# rtweet 0.2.91
* Added sample method for `stream_tweets()` function. By default,
the streaming query argument, `q`, is now set to an empty string,
`q = ""`, which returns a random sample of all Tweets
(pretty cool, right?).

# rtweet 0.2.9
* Added `post_tweet()` function. Users can now post tweets from their R console.

# rtweet 0.2.8
* Added `get_favorites()` function
* Update tests
* Exports tweets and users classes with show and plot methods

# rtweet 0.2.7
* Added screen_name variable for user mentions (in addition to user_id).

# rtweet 0.2.6

* Added `lookup_statuses()` function, which is the counterpart to
`lookup_users()`. Supply a vector of status IDs and return tweet data
for each status. `lookup_statuses()` is particularly powerful when
combined with other methods designed to collect older Tweets. Early
experiments with doing this all through R have turned out surprisingly
well, but packaging it in a way that makes it easy to do on other
machines is unlikely to happen in the short term.

* Removed dplyr dependencies. Everyone should install and use `dplyr`,
but for sake of parsimony, it's been removed from rtweet.

* Continued development of S4 classes and methods. Given removal of
dplyr dependencies, I've started to integrate print/show methods that
will limit the number of rows (and width of columns) when printed.
Given the amount of data returned in a relatively short period of time,
printing entire data frames quickly becomes headache-inducing.

# rtweet 0.2.5

* S4 class and methods integration

# rtweet 0.2.4

* Added new trends functions. Find what trending locations are
available with `trends_available()` and/or search for trends
worldwide or by geogaphical location using `get_trends()`.

* Stability improvements including integration with Travis CI and
code analysis via codecov. Token encryption method also means API
testing conducted on multiple machines and systems.

# rtweet 0.2.3

* Added new `search_users()` function! Search for users by keyword,
name, or interest and return data on the first 1000 hits.

# rtweet 0.2.2

* Output for `search_tweets()`, `stream_tweets()`, and
`get_timeline()` now consists of tweets data and contains users data
attribute.

* Output for `lookup_users()` now consists of users data and contains
tweets data attribute.

* To access users data from a tweets object or vice-versa, use
`users_data()` and `tweets_data()` functions on objects outputed
by major rtweet retrieval functions.

* Updated testthat tests

# rtweet 0.2.1

* Output for `get_friends()` and `get_followers()` is now a tibble
of "ids". To retrieve next cursor value, use new `next_cursor()`
function.

* Major stability improvements via testthat tests for every major
function.

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

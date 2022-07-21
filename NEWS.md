# rtweet 1.0.2

- Exported again `tweets_with_users` and `users_with_tweets` because Twitmo depends on them.

# rtweet 1.0.1

- Fixed issue with .Rbuilignore and vignettes

- Reduced fixtures sizes by limiting the page size to the number of requests if 
  it is smaller than the default page size. 

# rtweet 1.0.0

## Breaking changes

- Data returned by rtweet is nested and uses the same names provided by 
  the Twitter API. It doesn't compute or add new columns as it did previously. 

- emojis, langs and stopwordslangs data are no longer provided by rtweet.

- `get_friends()` and `get_followers()` return similar formatted output with 
  two columns "from_id" and "to_id" (#308, @alexpghayes).
  
- All paginated functions that don't return tweets now use a consistent 
  pagination interface. They all store the "next cursor" in an `rtweet_cursor`
  attribute, which will be automatically retrieved when you use the `cursor`
  argument.
  
- Functions that return tweets (e.g. `get_favorites()`, `get_my_timeline()`, 
  `get_timeline()`, `get_mentions()`, `lists_statuses()` and `search_tweets()`)
  now expose a consistent pagination interface. They all support `max_id` and 
  `since_id` to find earlier and later tweets respectively, as well as
  `retryonratelimit` to wait as long as needed when rate limited (#510).
  
- `suggested_slugs()`, `suggested_users()`, `suggested_users_all()` have been
  removed as they stopped working when Twitter remove the suggested users
  endpoint in June 2019 (https://twittercommunity.com/t/124732).
  
- `parse = FALSE` always means return the raw "JSON". Previously some functions
  (e.g. `my_friendships()`) would return the raw HTTP response instead (#504).
  
- rtweet no longer re-exports the magrittr pipe `%>%`; if you want to continue 
  using it, you'll need to `library(magrittr)` or `library(dplyr)` (#522).
  
## Deprecations

- The authentication system has been rewritten, check the following section.

- `lookup_collections()` and `get_collections()` has been hard deprecated 
  because the underlying Twitter API has been deprecated.

- `previous_cursor()` has been hard deprecated. It could only be used with 
  `lists_memberships()` and it has been dropped in favour of making regular
  pagination better.
  
- `tweet_shot()` has been hard deprecated as the screenshots do not have the 
  tweet. It might come back with webshot2 (#458).

- The `home` argument to `get_timeline()` has been deprecated. You can only
  retrieve the home timeline for the logged in user, and that's the job of
  `get_my_timeline()` (#550).
  
- Due to the changes on rtweet data format, all the functions related to 
  flattening the data (`write_as_csv()`, `save_as_csv()` `flatten()`, 
  `unflatten()`, `read_twitter_csv()`) are deprecated. 
  Users should decide how to flatten the nested structure of the data. 
  
- `lookup_statuses()` has been deprecated in favour of `lookup_tweets()`.

- `as_userid()` has been deprecated since in case of ambiguity the default is
  to assume a numeric string is a user id (#520). All functions now use a 
  single `user_type()` function so behaviour is identical for all rtweet 
  functions.

- `get_timelines()` has been deprecated since it does that same thing as
  `get_timeline()` (#509).
  
- `stream_tweets2()` has been deprecated in favour of `stream_tweets()`.

## Authentication

rtweet's authentication system has been completely written. It is now based 
around three authentication options: `rtweet_user()`, `rtweet_app()`, and
`rtweet_bot()`. Authentication no longer touches `~/.Renviron` file; instead
`auth_save()` and `auth_as()` allow you to explicitly save and load 
authentication mechanisms from a system config directory. See `vignette("auth")`
for more details.

- The httpuv package is now only suggested, since it's only needed for 
  interactive auth, and you'll be prompted to install it when needed.

- `bearer_token()` has been deprecated in favour of `rtweet_app()`, which takes 
  the bearer token found in your Twitter developer portal. `invalidate_bearer()`
  has been deprecated since this is something you should do yourself in the
  Twitter developer portal.

- `create_token()` has been deprecated in favour of the combination of
  `rtweet_user()`/`rtweet_bot()`/`rtweet_app()` + `auth_as()` + `auth_save()`.

- `get_token()` and `get_tokens()` have been deprecated in favour of 
  `auth_get()` and `auth_list()`.
  
- `auth_as()` accepts path to an authentication to make it easier to use
  authentications outside a user account (#602, @maelle)
  
- `auth_setup_default()` will not only authenticate and save but use the 
  default token.

- The new `auth_sitrep()` helps reports the different authentications of the user

## Other changes

- Update to new rOpenSci Code of Conduct: https://ropensci.org/code-of-conduct/

- `lookup_users()` and `search_users()` now returns a data frame containing
  all information about each user (not their latest tweet). If you want to get 
  that data you can use `tweets_data()`.
  
- rtweet 1.0.0 implements a consistent strategy for handling rate limits. 
  By default, if a paginated function (i.e. a rtweet function that performs 
  multiple calls to the twitter API) is rate-limited it will return all results 
  received up to that point, along with a warning telling you how to get more
  results. Alternatively, if you want to automatically wait until the 
  rate-limit is reset, you can set `retryratelimit = TRUE`.

- The default value of `retryonratelimit` comes from the option
  `rtweet.retryonratelimit` so you can globally set it to `TRUE` if desired
  (#173).
  
- All functions that perform multiple requests on your behalf now display
  a progress bar so you know what's happening. If you don't want it, you can 
  turn it off with `verbose = FALSE` (#518). 
  
- Banned or protected accounts now trigger a warning instead of an error, 
  but if data from other users is requested it is not served by the API and 
  returned as NA (#590, @simonheb).
  
- Added support for posting alt-text metadata with images tweeted with status 
  updated via `post_tweet()`. (#425, @hrbrmstr)
  
- `stream_tweets()` has been overhauled to only write valid data. This obsoletes
  all previous strategy to clean up bad data after the fact (#350, #356).

- The maintainer changed. 

- New `user_block()` and `user_unblock()` to block and unblock users (#593,
  @simonheb). 

- The new `tweet_threading` function is now faster and more reliable (#305, 
  #693, @charliejhadley). 

- Message are now properly capitalized (#565, @jsta)  

- Fields `withheld_scope`, `withheld_copyright`, `withheld_in_countries` are 
  now correctly parsed (#647, @alexpghayes).

- Functions like `search_tweets()`, `lookup_statuses()` and others return the 
   appropriate date time format for the right columns (`created_at` mostly) 
   (#653, #657, #660, @alexpghayes, @RickPat). 
   
- Premium/sandbox environments are supported in `search_fullarchive()` and 
   `search_30day()` (#578, #713).

- The vignette must be pre-computed before submission (#609, @maelle).
  
# rtweet 0.7.0

- Added paper.md as part of ROpenSci submission.
- Added contributing template.
- Added explanation of requirements and usage to `bearer_token()` docs.
- Transferred repo to ropensci
- Fixed numerous typos and grammatical mistakes (thank you several pull requests)
- More robust testing setup with encrypted keys entered via travis-ci web UI
- Data parsing: various bug fixes and stability improvements
- Added extended tweet mode to `list_statuses()` endpoint

# rtweet 0.6.9
- Better tweet-validating in streaming data–interrupted statuses/broken lines 
  are now returned
- Added network-graph convenience functions `network_data()` and `network_graph()`
- Added experimental support for premium APIs

# rtweet 0.6.8
- Users can now create read-only using the built-in rtweet client!

# rtweet 0.6.7
- `lookup_coords()` now requires a Google Maps API key. It will be stored for 
  easy future use once supplied.
- Improved documentation for authentication/token creation.
- Various bug fixes and improvements.

# rtweet 0.6.6
- Added `bearer_token()` option for access to more generous rate limits.
- Fixed issues with `create_token()` when using browse-based authentication 
  method.

# rtweet 0.6.5
- Added list management functionality via `post_list()`, which now allows users
  to create and populate lists as well as delete lists on behalf of one's own
  Twitter account.
- `lists_memberships()` and now scrolls through multiple pages of results to 
  automate collection of larger numbers of lists.
- Various bug fixes and improvements.

# rtweet 0.6.4
- Added new oauth method to `create_token()` which allows for creation of token
  non-interactive sessions via accepting inputs for consumer key, consumer 
  secret (always required), oauth key, and oauth secret (optional, if supplied
  then non-browser sign method is used).
- `ts_*()` functions now offer a `tz` (timezone) argument, allowing users to 
  more easily print and plot in non-UTC time.
- Users can now delete tweets by passing the status ID (of the desired tweet to
  be deleted) to the `destroy_id` argument in `post_tweet()`
- Various bug fixes and stability improvements.

# rtweet 0.6.3
- Fixed bug in `join_rtweet()`, which omitted users who didn't have 
  available tweets.
- Various bug fixes and stability improvements.

# rtweet 0.6.2
- Added `all_suggested_users()`, which automates the collection of Twitter's
  suggested users data.
- Various bug fixes and stability improvements.
- Significant upgrades to `save_as_csv()`, including addition of new 
  `prep_as_csv()` as convenience function for flattening Twitter data frames.
- Tokens have been retooled. For at least the time being, users must 
  create a Twitter app in order to be authorized to interact with the 
  REST and stream APIs.
- Joined data: instead of returning users/tweets data with its
  complementary tweets/users data stored as an attribute, functions now
  return a joined data frame, consisting of the tweets-level data 
  joined with the newest (most recent) observation for each user 
  This means functions now return a more consistent and intuitive 
  data object where one row is always equal to one tweet. 
- Overhauled `save_as_csv()` with improved flattening and ID-preserving 
  saving methods. The function now saves a single [joined] data set as 
  well.
- Fixed major bugs in `get_favorites()` and in several `lists_*()` 
  functions.
- Tweaked date-time aggregator internals to make time-rounding more 
  precise.

# rtweet 0.6.0
- Introduced new API authorization method, which leverages an embedded
  rtweet Twitter app that is authorized locally by the user. Creating
  Twitter apps is non longer necessary. Users need only click "okay"
  to create and store their API authorization token.
- Improved parsing and line-reading internals for `stream_tweets()`
- Added `stream_tweets2()` function for more robust streaming
  method. Streams JSON files to directory and reconnects following
  premature disruptions.
- Various bug fixes and numerous documentation improvements.

# rtweet 0.5.0
- Added access to direct messages, mentions, list subscriptions, list
  users, list members, and list memberships
- Various fixes to parsing, integrating tibble for output, and
  streaming geolocation-related functions and data.
- Fixed issues with streaming and parsing streamed data.

# rtweet 0.4.9
- Functions `get_timeline()`, `get_favorites()`, `get_friends()`, and
 `get_followers()` now accept vectors of length > 1.
- Fixed bugs related to users data and its extracter, `users_data()`
- New stream parser, `stream_data()`, designed to parse files that cannot
  wholely fit into memory. `stream_data()` can now work in parallel as well.

# rtweet 0.4.8
- Support for additional APIs has been added--including APIs designed
  to return information related to lists and retweets.
- The `post_status()` function has been fixed and can now be used to
  upload media.
- Several adjustments have been made in response to various changes in
  Twitter's APIs.
- Thanks to all the great feedback on Github, numerous bug fixes
  and improvements have been included as well. In general, things
  should become a lot more stable across functions and data
  structures.

# rtweet 0.4.7
- The relatively lightweight tibble package is now a package dependency.
- Speed boosts to parsing process. It's possible to convert from json to
  data frames in parallel, but I'm not sure minimal gains are worth the
  headache. Regardless, the current version should return more data,
  more reliably, and faster.
- By default, functions now return data frames (tibbles) with recursive
  lists (e.g., the 3rd observation of `mentions_screen_name` may consist of
  4 screen names).
- To revert back to the flattened/delim object, use the `flatten()` function.
  Exporting functions such as `save_as_csv` will apply flatten by default.
- Three different sets of coordinate variables are now returned: `coords_coords`,
  `geo_coords`, and `bbox_coords` bounding box. The first two come in
  pairs of coords (a list column) and bbox_coords comes with 8
  values (longX4 latX4). This should allow users to maximize returns
  on
  geo-location data.

# rtweet 0.4.6
- More efficient iterations through pages of results.
- Added to documentation, including new package documentation domain:
  http://rtweet.info.
- Improvements made in collecting and using geo data.

# rtweet 0.4.5
- Convenience function `plain_tweets()` added for textual analysis.
- Overhaul of `ts_plot()` with improved time-aggregating method. Now a
  wrapper around `ts_data()`, deprecating `ts_filter`.

# rtweet 0.4.4
- Lots of query-building features added to search tweets, including
  ability to search by geolocation.
- Post actions now include replying to status ID.
- Other various bug fixes and speed improvements.

# rtweet 0.4.3
- Now returns tibbles (tibble is a recommended dependency)
- Various bug fixes and code improvements.

# rtweet 0.4.2
* Various bug fixes
* Integration with ggplot2 as a suggested dependency

# rtweet 0.4.1
* Fixed bugs with `mutate_coords()` and `retryonratelimit`.
* Now returns full text of tweets exceeding 140 characters. This
  change was necessary due to recent changes in Twitter's API.

# rtweet 0.4.0
* CRAN release featuring major additions to documentation and support in
  addition to new and improved functions like `ts_plot()`, `ts_filter()`
  and more!

# rtweet 0.3.96
* For dev: added package builder for better versioning and
  more frequent updates to NEWS.md file.
* Added new live streaming vignette as well as updated
  and improved tokens vignette
* Various bug fixes and improvements to tokens, parse, and plot functions.

# rtweet 0.3.93
* All interactive/posting functions have been modified with the prefix
  `post_`. This was done to clearly distinguish write functions from
  retrieval functions.
* More bug fixes and various improvements.
* The `ts_plot()` function is now more robust with more adaptive
  characteristics for variations in the number of filters, the method
  of distinguishing lines, the position of the legend, and the
  aesthetics of the themes.
* Added `ts_filter()` function which allows users to convert Twitter
  data into a time series-like data frame. Users may also provide
  filtering rules with which `ts_filter()` will subset the data as it
  converts it to multiple time series, which it then outputs as a
  long-form (tidy) data frame.

# rtweet 0.3.92

* `search_tweets` now includes `retryonratelimit` argument to
allow for searches requesting more than 18,000 tweets. This
automates what was previously possible through use of `max_id`.
* Various bug fixes and improvements to parsing and pagination-
assisting functions.
* Fixed bug in encoding with `stream_tweets`.

# rtweet 0.3.91

* Major improvements to ts_plot including SIX different
themes from which users may choose
* More parsing fixes and misc stability improvements
* Minor renaming of variables along with returning more
variables overall

# rtweet 0.3.9

* Fixes minor problems with `parse.piper` function
* More additions to plotting and data wrangling for the
purpose of plotting

# rtweet 0.3.8

* Functions by default use a new faster parser that returns more
variables
* Text analysis functions provided for convenience
* Plotting with maps
* Tidyverse consistencies

# rtweet 0.3.8

* Fixed issue with geo tracking in stream_tweets
* Various bug fixes and stability improvements

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
worldwide or by geographical location using `get_trends()`.

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
`users_data()` and `tweets_data()` functions on objects output by major 
rtweet retrieval functions.

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

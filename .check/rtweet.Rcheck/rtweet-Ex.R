pkgname <- "rtweet"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "rtweet-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('rtweet')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("get_favorites_call")
### * get_favorites_call

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_favorites_call
### Title: get_favorites_call
### Aliases: get_favorites_call

### ** Examples

## Not run: 
##D # get favorites of the president of the US
##D pres <- get_favorites(user = "potus")
##D pres
##D 
##D # get favorites of the Environmental Protection Agency
##D epa <- get_favorites(user = "epa")
##D epa
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_favorites_call", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_followers.default")
### * get_followers.default

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_followers.default
### Title: get_followers
### Aliases: get_followers.default

### ** Examples

## Not run: 
##D # get ids of users following the president of the US
##D pres <- get_followers(user = "potus")
##D pres
##D 
##D # get ids of users following the Environmental Protection Agency
##D epa <- get_followers(user = "epa")
##D epa
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_followers.default", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_friends.default")
### * get_friends.default

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_friends.default
### Title: get_friends
### Aliases: get_friends.default

### ** Examples

## Not run: 
##D # get ids of users followed by the president of the US
##D pres <- get_friends(users = "potus")
##D pres
##D 
##D # get friend networks of multiple users
##D epa <- get_friends(users = c("jack", "epa"))
##D epa
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_friends.default", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_timeline_")
### * get_timeline_

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_timeline_
### Title: get_timeline
### Aliases: get_timeline_

### ** Examples

## Not run: 
##D # get 2000 from Donald Trump's account
##D djt <- get_timeline("realDonaldTrump", n = 2000)
##D 
##D # data frame where each observation (row) is a different tweet
##D djt
##D 
##D # users data for realDonaldTrump is also retrieved.
##D # access it via users_data() users_data(hrc)
##D users_data(djt)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_timeline_", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_timelines2")
### * get_timelines2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_timelines2
### Title: get timelines
### Aliases: get_timelines2

### ** Examples

## Not run: 
##D rt <- get_timelines(
##D   users = c("hadleywickham", "hspter", "rdpeng", "calbon", "dataandme"),
##D   n = 400
##D )
##D if (requireNamespace("dplyr", quietly = TRUE)) {
##D   rt %>%
##D     dplyr::group_by(screen_name) %>%
##D     word_n() %>%
##D     dplyr::filter(!word %in% stopwords) %>%
##D     dplyr::group_by(screen_name) %>%
##D     dplyr::top_n(10, n) %>%
##D     print(n = 50)
##D } else {
##D   rt %>%
##D     word_n("screen_name") %>%
##D     subset(!word %in% stopwords & n > 8) %>%
##D     print(n = 50)
##D }
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_timelines2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_trends")
### * get_trends

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_trends
### Title: GET trends/place
### Aliases: get_trends

### ** Examples

## Not run: 
##D # Retrieve available trends
##D trends <- available_trends()
##D trends
##D 
##D # Store WOEID for Worldwide trends
##D worldwide <- subset(trends, name == "Worldwide")[["woeid"]]
##D 
##D # Retrieve worldwide trends datadata
##D ww_trends <- get_trends(woeid = worldwide)
##D 
##D # Preview trends data
##D ww_trends
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_trends", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_trends_closest")
### * get_trends_closest

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_trends_closest
### Title: get_trends_closest
### Aliases: get_trends_closest

### ** Examples

## Not run: 
##D 
##D # Retrieve trends data for latitude, longitude near New York City
##D nyc_trends <- get_trends_closest(lat = 40.7, long=-74.0, token=twitter_token)
##D 
##D # Preview trends data
##D nyc_trends
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_trends_closest", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("lookup_coords")
### * lookup_coords

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: lookup_coords
### Title: lookup_coords
### Aliases: lookup_coords

### ** Examples

## Not run: 
##D sf <- lookup_coords("san francisco, CA", "country:US")
##D usa <- lookup_coords("usa")
##D lnd <- lookup_coords("london", box = FALSE)
##D bz <- lookup_coords("brazil")
##D 
##D search_tweets(geocode = bz)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("lookup_coords", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("lookup_statuses")
### * lookup_statuses

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: lookup_statuses
### Title: lookup_tweets
### Aliases: lookup_statuses lookup_tweets

### ** Examples

## Not run: 
##D # lookup tweets data via status_id vector
##D statuses <- c("567053242429734913", "266031293945503744",
##D   "440322224407314432")
##D statuses <- lookup_statuses(statuses)
##D statuses
##D 
##D # view users data for these statuses via tweets_data()
##D users_data(statuses)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("lookup_statuses", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("lookup_users")
### * lookup_users

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: lookup_users
### Title: lookup_users
### Aliases: lookup_users

### ** Examples

## Not run: 
##D ## lookup vector of 1 or more user_id or screen_name
##D users <- c("potus", "hillaryclinton", "realdonaldtrump",
##D   "fivethirtyeight", "cnn", "espn", "twitter")
##D 
##D ## get users data
##D usr_df <- lookup_users(users)
##D 
##D ## view users data
##D usr_df
##D 
##D ## view tweet data for these users via tweets_data()
##D tweets_data(usr_df)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("lookup_users", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("next_cursor")
### * next_cursor

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: next_cursor
### Title: next_cursor
### Aliases: next_cursor next_page cursor_next family ids

### ** Examples

## Not run: 
##D ## Retrieve user ids of accounts following POTUS
##D f1 <- get_followers("potus", n = 75000)
##D 
##D ## store next_cursor in page
##D page <- next_cursor(f1)
##D 
##D ## max. number of ids returned by one token is 75,000 every 15
##D ##   minutes, so you'll need to wait a bit before collecting the
##D ##   next batch of ids
##D sys.Sleep(15 * 60) ## Suspend execution of R expressions for 15 mins
##D 
##D ## Use the page value returned from \code{next_cursor} to continue
##D ##   where you left off.
##D f2 <- get_followers("potus", n = 75000, page = page)
##D 
##D ## combine
##D f <- do.call("rbind", list(f1, f2))
##D 
##D ## count rows
##D nrow(f)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("next_cursor", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("parse_stream")
### * parse_stream

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: parse_stream
### Title: parse_stream
### Aliases: parse_stream

### ** Examples

## Not run: 
##D ## file extension automatically converted to .json whether or
##D ## not file_name already includes .json
##D stream_tweets(q = "", timeout = 30,
##D               file_name = "rtweet-stream", parse = FALSE)
##D rt <- parse_stream("rtweet-stream.json")
##D ## preview tweets data
##D head(rt)
##D ## preview users data
##D head(users_data(rt))
##D ## plot time series
##D ts_plot(rt, "secs")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("parse_stream", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("post_favorite")
### * post_favorite

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: post_favorite
### Title: post_favorite
### Aliases: post_favorite post_favourite favorite_tweet

### ** Examples

## Not run: 
##D rt <- search_tweets("rstats")
##D r <- lapply(rt$user_id, post_favorite)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("post_favorite", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("post_follow")
### * post_follow

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: post_follow
### Title: post_follow
### Aliases: post_follow follow_user

### ** Examples

## Not run: 
##D post_follow("BarackObama")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("post_follow", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("post_tweet")
### * post_tweet

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: post_tweet
### Title: post_tweet
### Aliases: post_tweet post_status

### ** Examples

## Not run: 
##D x <- rnorm(300)
##D y <- x + rnorm(300, 0, .75)
##D col <- c(rep("#002244aa", 50), rep("#440000aa", 50))
##D bg <- c(rep("#6699ffaa", 50), rep("#dd6666aa", 50))
##D tmp <- tempfile(fileext = "png")
##D png(tmp, 6, 6, "in", res = 127.5)
##D par(tcl = -.15, family = "Inconsolata",
##D     font.main = 2, bty = "n", xaxt = "l", yaxt = "l",
##D     bg = "#f0f0f0", mar = c(3, 3, 2, 1.5))
##D plot(x, y, xlab = NULL, ylab = NULL, pch = 21, cex = 1,
##D      bg = bg, col = col,
##D      main = "This image was uploaded by rtweet")
##D grid(8, lwd = .15, lty = 2, col = "#00000088")
##D dev.off()
##D browseURL(tmp)
##D post_tweet(".Call(\"oops\", ...)",
##D            media = tmp)
##D 
##D # example of replying within a thread
##D post_tweet(status="first in a thread")
##D my_timeline <- get_timeline(self_user_name, n=1, token=twitter_token)
##D reply_id <- my_timeline[1,]$status_id
##D post_tweet(status="second in the thread", in_reply_to_status_id=reply_id)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("post_tweet", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rtweet")
### * rtweet

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rtweet
### Title: rtweet
### Aliases: rtweet tokens rtwitter rtweets rtweet-package

### ** Examples

## Not run: 
##D ## for instructions on access tokens, see the tokens vignette
##D vignette("auth")
##D 
##D ## for a quick demo check the rtweet vignette
##D vignette("rtweet")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rtweet", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("search_tweets.default")
### * search_tweets.default

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: search_tweets.default
### Title: search_tweets.default
### Aliases: search_tweets.default

### ** Examples

## Not run: 
##D ## search for 1000 tweets mentioning Hillary Clinton
##D hrc <- search_tweets(q = "hillaryclinton", n = 1000)
##D 
##D ## data frame where each observation (row) is a different tweet
##D hrc
##D 
##D ## users data also retrieved. can access it via users_data()
##D users_data(hrc)
##D 
##D ## search for 1000 tweets in English
##D djt <- search_tweets(q = "realdonaldtrump", n = 1000, lang = "en")
##D djt
##D users_data(djt)
##D 
##D ## exclude retweets
##D rt <- search_tweets("rstats", n = 500, include_rts = FALSE)
##D 
##D ## perform search for lots of tweets
##D rt <- search_tweets("trump OR president OR potus", n = 100000,
##D                     retryonratelimit = TRUE)
##D 
##D ## plot time series of tweets frequency
##D ts_plot(rt, by = "mins", theme = "spacegray",
##D         main = "Tweets about Trump")
##D 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("search_tweets.default", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("search_users")
### * search_users

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: search_users
### Title: search_users
### Aliases: search_users

### ** Examples

## Not run: 
##D # search for 1000 tweets mentioning Hillary Clinton
##D pc <- search_users(q = "political communication", n = 1000)
##D 
##D # data frame where each observation (row) is a different user
##D pc
##D 
##D # tweets data also retrieved. can access it via tweets_data()
##D users_data(hrc)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("search_users", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("search_users_call")
### * search_users_call

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: search_users_call
### Title: search_users
### Aliases: search_users_call

### ** Examples

## Not run: 
##D # search for 1000 tweets mentioning Hillary Clinton
##D pc <- search_users(q = "political communication", n = 1000)
##D 
##D # data frame where each observation (row) is a different user
##D pc
##D 
##D # tweets data also retrieved. can access it via tweets_data()
##D users_data(hrc)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("search_users_call", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("stream_tweets")
### * stream_tweets

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: stream_tweets
### Title: stream_tweets
### Aliases: stream_tweets

### ** Examples

## Not run: 
##D ## stream tweets mentioning "election" for 90 seconds
##D e <- stream_tweets("election", timeout = 90)
##D 
##D ## data frame where each observation (row) is a different tweet
##D head(e)
##D 
##D ## users data also retrieved, access it via users_data()
##D users_data(e) %>%
##D     head()
##D 
##D ## plot tweet frequency
##D ts_plot(e, "secs")
##D 
##D ## stream tweets mentioning Obama for 30 seconds
##D djt <- stream_tweets("realdonaldtrump", timeout = 30)
##D 
##D ## preview tweets data
##D head(djt)
##D 
##D ## get user IDs of people who mentioned trump
##D usrs <- users_data(djt)
##D 
##D ## lookup users data
##D usrdat <- lookup_users(unique(usrs$user_id))
##D 
##D ## preview users data
##D head(usrdat)
##D 
##D ## store large amount of tweets in files using continuous streams
##D ## by default, stream_tweets() returns a random sample of all tweets
##D ## leave the query field blank for the random sample of all tweets.
##D stream_tweets(timeout = (60 * 10), parse = FALSE,
##D     file_name = "tweets1")
##D stream_tweets(timeout = (60 * 10), parse = FALSE,
##D     file_name = "tweets2")
##D 
##D ## parse tweets at a later time using parse_stream function
##D tw1 <- parse_stream("tweets1.json")
##D head(tw1)
##D 
##D tw2 <- parse_stream("tweets2.json")
##D head(tw2)
##D 
##D ## streaming tweets by specifying lat/long coordinates
##D 
##D ## stream continental US tweets for 5 minutes
##D usa <- stream_tweets(
##D     c(-125,26,-65,49),
##D     timeout = 300)
##D 
##D ## stream world tweets for 5 mins, save to json file
##D world.old <- stream_tweets(
##D     c(-180, -90, 180, 90),
##D     timeout = (60 * 5),
##D     parse = FALSE,
##D     file_name = "world-tweets.json")
##D 
##D ## read in json file
##D rtworld <- parse_stream("word-tweets.json")
##D 
##D ## word with coords
##D x <- mutate.coords(rtworld)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("stream_tweets", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("trends_available")
### * trends_available

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: trends_available
### Title: trends_available
### Aliases: trends_available

### ** Examples

## Not run: 
##D # Retrieve available trends
##D trends <- available_trends()
##D trends
##D 
##D # Store WOEID for Worldwide trends
##D worldwide <- subset(trends, name == "Worldwide")[["woeid"]]
##D 
##D # Retrieve worldwide trends datadata
##D ww_trends <- get_trends(woeid = Worldwide)
##D 
##D # Preview Worldwide trends data
##D ww_trends
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("trends_available", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("trends_closest")
### * trends_closest

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: trends_closest
### Title: Get closest available location to given Lat, Long
### Aliases: trends_closest

### ** Examples

## Not run: 
##D # New York is ~ 40.7N, 74.0W
##D nyc_woeid <- trends_closest(40.7, -74.0, twitter_token)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("trends_closest", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tweets_data")
### * tweets_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tweets_data
### Title: tweets_data
### Aliases: tweets_data tweet_data data_tweet data_tweets

### ** Examples

## Not run: 
##D ## get twitter user data
##D jack <- lookup_users("jack")
##D 
##D ## get data on most recent tweet from user(s)
##D tweets_data(jack)
##D 
##D ## search for 100 tweets containing the letter r
##D r <- search_tweets("r")
##D 
##D ## print tweets data (only first 10 rows are shown)
##D head(r, 10)
##D 
##D ## preview users data
##D head(users_data(r))
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tweets_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("users_data")
### * users_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: users_data
### Title: users_data
### Aliases: users_data user_data data_user data_users

### ** Examples

## Not run: 
##D ## search for 100 tweets containing the letter r
##D r <- search_tweets("r")
##D 
##D ## print tweets data (only first 10 rows are shown)
##D head(r, 10)
##D 
##D ## extract users data
##D head(users_data(r))
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("users_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

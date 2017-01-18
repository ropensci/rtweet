## ---- eval=FALSE---------------------------------------------------------
#  install.packages("rtweet")
#  library(rtweet)

## ------------------------------------------------------------------------
paste0("your lucky number is ", sample(1:20, 1))

## ------------------------------------------------------------------------
c(1:20) %>%
    sample(1) %>%
    paste0("your lucky number is ", .)

## ------------------------------------------------------------------------
## select the four element from the lhs of the pipe
c(1:10) %>%
    .[[4]]

## ------------------------------------------------------------------------
c(1:10) %>%
    mean()

## ---- eval=FALSE---------------------------------------------------------
#  ## search for 500 tweets using the #rstats hashtag
#  team_rstats <- search_tweets("#rstats", n = 500)
#  team_rstats
#  
#  ## access and preview data on the users who posted the tweets
#  users_data(team_rstats) %>%
#      head()
#  
#  ## return 200 tweets from @KyloR3n's timeline
#  kylo_is_a_mole <- get_timeline("KyloR3n", n = 2000)
#  head(kylo_is_a_mole)
#  
#  ## extract emo kylo ren's user data
#  users_data(kylo_is_a_mole)
#  
#  ## stream tweets mentioning @HillaryClinton for 2 minutes (120 sec)
#  imwithher <- stream_tweets("HillaryClinton", timeout = 120)
#  head(imwithher)
#  
#  ## extract data on the users who posted the tweets
#  head(users_data(imwithher))
#  
#  ## stream 3 random samples of tweets
#  for (in in seq_len(3)) {
#  	stream_tweets(q = "", timeout = 60,
#  		file_name = paste0("rtw", i), parse = FALSE)
#  	if (i == 3) {
#  		message("all done!")
#  		break
#  	} else {
#  		# wait between 0 and 300 secs before next stream
#  		Sys.sleep(runif(1, 0, 300))
#  	}
#  }
#  
#  ## parse the samples
#  tw <- lapply(c("rtw1.json", "rtw2.json", "rtw3.json"),
#               parse_stream)
#  
#  ## collapse lists into single data frame
#  tw.users <- do.call("rbind", users_data(tw))
#  tw <- do.call("rbind", tw)
#  attr(tw, "users") <- tw.users
#  
#  ## preview data
#  head(tw)
#  users_data(tw) %>%
#      head()

## ---- eval=FALSE---------------------------------------------------------
#  # search for 500 users using "social science" as a keyword
#  harder_science <- search_users("social science", n = 500)
#  harder_science
#  
#  # extract most recent tweets data from the social scientists
#  tweets_data(harder_science)
#  
#  ## lookup users by screen_name or user_id
#  users <- c("KimKardashian", "justinbieber", "taylorswift13",
#             "espn", "JoelEmbiid", "cstonehoops", "KUHoops",
#             "upshotnyt", "fivethirtyeight", "hadleywickham",
#             "cnn", "foxnews", "msnbc", "maddow", "seanhannity",
#             "potus", "epa", "hillaryclinton", "realdonaldtrump",
#             "natesilver538", "ezraklein", "annecoulter")
#  famous_tweeters <- lookup_users(users)
#  
#  ## preview users data
#  famous_tweeters
#  
#  # extract most recent tweets data from the famous tweeters
#  tweets_data(famous_tweeters)
#  
#  ## or get user IDs of people following stephen colbert
#  colbert_nation <- get_followers("stephenathome", n = 18000)
#  
#  ## get even more by using the next_cursor function
#  page <- next_cursor(colbert_nation)
#  
#  ## use the page object to continue where you left off
#  colbert_nation_ii <- get_followers("stephenathome", n = 18000, page = page)
#  colbert_nation <- c(unlist(colbert_nation), unlist(colbert_nation_ii))
#  
#  ## and then lookup data on those users (if hit rate limit, run as two parts)
#  colbert_nation <- lookup_users(colbert_nation)
#  colbert_nation
#  
#  ## or get user IDs of people followed *by* President Obama
#  obama1 <- get_friends("BarackObama")
#  obama2 <- get_friends("BarackObama", page = next_cursor(obama1))
#  
#  ## and lookup data on Obama's friends
#  lookup_users(c(unlist(obama1), unlist(obama2)))

## ---- eval=FALSE---------------------------------------------------------
#  ## get trending hashtags, mentions, and topics worldwide
#  prestige_worldwide <- get_trends()
#  prestige_worldwide
#  
#  ## or narrow down to a particular country
#  usa_usa_usa <- get_trends("United States")
#  usa_usa_usa
#  
#  ## or narrow down to a popular city
#  CHIEFS <- get_trends("Kansas City")
#  CHIEFS

## ---- eval=FALSE---------------------------------------------------------
#  post_tweet("my first rtweet #rstats")


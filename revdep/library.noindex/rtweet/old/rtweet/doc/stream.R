## ---- include=FALSE------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, eval = FALSE, comment = "#>", collapse = TRUE)

## ------------------------------------------------------------------------
#  ## Install rtweet
#  install.packages("rtweet")
#  ## Load rtweet
#  library(rtweet)

## ------------------------------------------------------------------------
#  ## Stream keywords used to filter tweets
#  q <- "hillaryclinton,imwithher,realdonaldtrump,maga,electionday"
#  
#  ## Stream time in seconds so for one minute set timeout = 60
#  ## For larger chunks of time, I recommend multiplying 60 by the number
#  ## of desired minutes. This method scales up to hours as well
#  ## (x * 60 = x mins, x * 60 * 60 = x hours)
#  ## Stream for 30 minutes
#  streamtime <- 30 * 60
#  
#  ## Filename to save json data (backup)
#  filename <- "rtelect.json"

## ------------------------------------------------------------------------
#  ## Stream election tweets
#  rt <- stream_tweets(q = q, timeout = streamtime, file_name = filename)

## ------------------------------------------------------------------------
#  ## No upfront-parse save as json file instead method
#  stream_tweets(
#    q = q,
#    parse = FALSE,
#    timeout = streamtime,
#    file_name = filename
#  )
#  ## Parse from json file
#  rt <- parse_stream(filename)
#  
#  ## stream_tweets2 method
#  twoweeks <- 60L * 60L * 24L * 7L * 2L
#  congress <- "congress,senate,house of representatives,representatives,senators,legislative"
#  stream_tweets2(
#    q = congress,
#    parse = FALSE,
#    timeout = twoweeks,
#    dir = "congress-stream"
#  )
#  
#  ## Parse from json file
#  rt <- parse_stream("congress-stream.json")

## ------------------------------------------------------------------------
#  ## Preview tweets data
#  rt

## ------------------------------------------------------------------------
#  ## Preview users data
#  users_data(rt)

## ------------------------------------------------------------------------
#  ## Plot time series of all tweets aggregated by second
#  ts_plot(rt, by = "secs")

## ------------------------------------------------------------------------
#  ## plot multiple time series by first grouping the data by screen name
#  rt %>%
#    dplyr::group_by(screen_name) %>%
#    ts_plot() +
#    ggplot2::labs(
#      title = "Tweets during election day for the 2016 U.S. election",
#      subtitle = "Tweets collected, parsed, and plotted using `rtweet`"
#    )


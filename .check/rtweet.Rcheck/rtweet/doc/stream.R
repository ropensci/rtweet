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
#  q <- paste0("hillaryclinton,imwithher,realdonaldtrump,maga,electionday")
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
#  ## Name of dedicated rtweet data directory
#  rtweet.folder <- path.expand("~/rtweet-data")
#  
#  ## Create dedicated rtweet data directory
#  if (!dir.exists(rtweet.folder)) {
#      dir.create(rtweet.folder)
#  }
#  
#  ## Name streaming file based on time of stream
#  ## Combine with directory to form path
#  filename <- file.path(
#      rtweet.folder, paste0(format(Sys.time(), "%F-%H-%S"), ".json"))
#  
#  ## create meta data
#  metadata <- paste0(
#      "q = ", q, "\n",
#      "streamtime = ", streamtime, "\n",
#      "filename = ", filename)
#  
#  ## create file name for stream's meta data
#  metafile <- gsub(".json$", ".txt", filename)
#  
#  ## save meta data
#  cat(metadata, file = metafile)

## ------------------------------------------------------------------------
#  ## No upfront-parse save as json file instead method
#  stream_tweets(q = q, parse = FALSE,
#                timeout = streamtime,
#                file_name = filename)
#  ## Parse from json file
#  rt <- parse_stream(filename)

## ------------------------------------------------------------------------
#  ## Preview tweets data
#  head(rt)

## ------------------------------------------------------------------------
#  ## Preview users data
#  users_data(rt) %>%
#       head()

## ------------------------------------------------------------------------
#  ## Plot time series of all tweets aggregated by second
#  ts_plot(rt, by = "secs")

## ------------------------------------------------------------------------
#  ## plot multiple time series by first filtering the data using
#  ## regular expressions on the tweet "text" variable
#  rt %>%
#      ts_filter(
#          by = "mins",
#          filter = c("hillary|clinton|imwithher",
#                     "donald|trump|maga",
#                     "vot|democracy"),
#          key = c("Clinton", "Trump", "Democracy"),
#          trim = TRUE) %>%
#      ## The pipe operator allows you to combine this with ts_plot
#      ## without things getting too messy.
#      ts_plot(
#          theme = "spacegray",
#          cols = c("#6699ee", "#dd7a7a", "#7acc7a"),
#          main = "Tweets during election day for the 2016 U.S. election",
#          subtitle = "Tweets collected, parsed, and plotted using `rtweet`")


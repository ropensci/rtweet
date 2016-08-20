## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(comment = "#>", collapse = TRUE)

## ---- eval=FALSE---------------------------------------------------------
#  # install from CRAN
#  install.packages("rtweet")
#  
#  # load rtweet
#  library(rtweet)

## ---- eval=FALSE---------------------------------------------------------
#  twitter_tokens <- c(
#    create_token(app = "rtweet_tokens", #whatever you named your app
#      consumer_key = "XZgqotgOZNKlLFJqFbd8NjUtL",
#      consumer_secret = "1rDnU3H3nrxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
#    create_token(app = "rtweet_roauth",
#      consumer_key = "XZgqoxxxxxxxxxxxxxxxxxxxx",
#      consumer_secret = "1rDnU3H3nrxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
#    )
#  # (xxxxx's added but you get the point)

## ---- echo = TRUE, eval = FALSE------------------------------------------
#  home_directory <- normalizePath("~/")
#  file_name <- paste0(home_directory, "/", "twitter_tokens")
#  save(twitter_tokens, file = file_name)

## ---- eval=FALSE---------------------------------------------------------
#  cat("TWITTER_PAT=/Users/mwk/twitter_tokens\n",
#    file = paste0(home_directory, "/.Renviron"),
#    append = TRUE)


## ---- include=FALSE------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, eval = FALSE, comment = "#>", collapse = TRUE)

## ------------------------------------------------------------------------
#  ## install httpuv if not already
#  if (!requireNamespace("httpuv", quietly = TRUE)) {
#    install.packages("httpuv")
#  }

## ------------------------------------------------------------------------
#  ## autheticate via web browser
#  token <- create_token(
#    app = "rtweet_token",
#    consumer_key = "XYznzPFOFZR2a39FwWKN1Jp41",
#    consumer_secret = "CtkGEWmSevZqJuKl6HHrBxbCybxI1xGLqrD5ynPd9jG0SoHZbD")

## ------------------------------------------------------------------------
#  ## check to see if the token is loaded
#  identical(twitter_token, get_token())

## ------------------------------------------------------------------------
#  ## authenticate via access token
#  token <- create_token(
#    app = "my_twitter_research_app",
#    consumer_key = "XYznzPFOFZR2a39FwWKN1Jp41",
#    consumer_secret = "CtkGEWmSevZqJuKl6HHrBxbCybxI1xGLqrD5ynPd9jG0SoHZbD",
#    acess_token = "9551451262-wK2EmA942kxZYIwa5LMKZoQA4Xc2uyIiEwu2YXL",
#    access_secret = "9vpiSGKg1fIPQtxc5d5ESiFlZQpfbknEN1f1m2xe5byw7")

## ------------------------------------------------------------------------
#  ## check to see if the token is loaded
#  identical(twitter_token, get_token())


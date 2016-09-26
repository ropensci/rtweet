## ---- eval=FALSE---------------------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("mkearney/rtweet")
#  library(rtweet)

## ---- eval=FALSE---------------------------------------------------------
#  d <- stream_tweets(q = "debates2016", timeout = (60 * 10))

## ---- eval=FALSE---------------------------------------------------------
#  # plot time series
#  ts_plot(d, by = "15 secs")


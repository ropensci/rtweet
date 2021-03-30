#' rtweet: Collect Twitter data from R
#'
#' @description 
#' rtweet provides users a range of functions designed to extract data
#' from Twitter's REST and streaming APIs. It has three main goals:
#'
#' * Formulate and send requests to Twitter's REST and stream APIs.
#' * Retrieve and iterate over returned data.
#' * Wrangling data into tidy structures.
#' 
#' Get started by reading `vignette("rtweet")`.
#' 
#' @keywords internal
"_PACKAGE"

#' @import rlang
NULL

# The internal environment where state of the app is stored.
.state <- new.env(parent = emptyenv())

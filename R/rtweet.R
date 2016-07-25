#' @title rtweet: collecting Twitter data
#'
#' @description rtweet provides users a range of functions
#' designed to extract data from Twitter's REST and
#' streaming APIs.
#'
#' It has three main goals:
#'
#' \itemize{
#' \item Compose and request calls to
#'   \url{https://api.twitter.com} following (the best
#'   practices for writing an API package)
#'   [https://cran.r-project.org/web/packages/httr/
#'   vignettes/api-packages.html].
#' \item Provide functions that can acccept multiple
#'   personal access tokens.
#' \item Make extracting data from Twitter seem a little
#'   easier.
#' }
#'
#' To learn more about rtweet, start with the vignette:
#' \code{browseVignettes(package = "rtweet")}
#'
#' @docType package
#' @name rtweet
#' @import httr
#' @importFrom jsonlite fromJSON
NULL

.state <- new.env(parent = emptyenv())

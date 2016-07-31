#' @title rtweet
#'
#' @description rtweet provides users a range of functions
#'   designed to extract data from Twitter's REST and
#'   streaming APIs.
#'
#' It has three main goals:
#'
#' \itemize{
#' \item Compose and request calls to Twitter's API
#'   following (the best practices for writing an API
#'   package) [https://cran.r-project.org/web/packages/httr/
#'   vignettes/api-packages.html].
#' \item Provide functions that can acccept multiple
#'   personal access tokens.
#' \item Make extracting data from Twitter seem a little
#'   easier.
#' }
#'
#' @docType package
#' @name rtweet
#' @import httr jsonlite dplyr
NULL

.state <- new.env(parent = emptyenv())

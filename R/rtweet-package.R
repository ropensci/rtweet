#' @title rtweet
#'
#' @description rtweet provides users a range of functions
#'   designed to extract data from Twitter's REST and
#'   streaming APIs.
#'
#' It has three main goals:
#'
#' \itemize{
#' \item Formulate and send requests to Twitter's REST and stream APIs.
#' \item Retrieve and iterate over returned data.
#' \item Wrangling data into tidy structures.
#' }
#' @examples
#' \dontrun{
#' ## for instructions on access tokens, see the tokens vignette
#' vignette("auth")
#'
#' ## for a quick demo check the rtweet vignette
#' vignette("rtweet")
#' }
#'
#' @docType package
#' @aliases tokens rtwitter rtweets
#' @name rtweet
NULL

.onAttach <- function(libname, pkgname) {
    packageStartupMessage("Welcome to rtweet v0.4.8!")
}

.state <- new.env(parent = emptyenv())

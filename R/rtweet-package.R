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
#'   package) [https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html].
#' \item Provide functions that can acccept multiple
#'   personal access tokens.
#' \item Make extracting data from Twitter seem a little
#'   easier.
#' }
#' @examples
#' \dontrun{
#' # for instructions on access tokens, see the tokens vignette
#' vignette("tokens")
#'
#' # for a quick demo check the rtweet vignette
#' vignette("rtweet")
#' }
#'
#'
#' @docType package
#' @aliases tokens rtwitter rtweets
#' @name rtweet
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to rtweet v0.3.0!")
}

.state <- new.env(parent = emptyenv())

#' rtweet: Collecting Twitter data
#'
#' rtweet provides users a range of functions designed to extract data
#' from Twitter's REST and streaming APIs.
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
"_PACKAGE"

.state <- new.env(parent = emptyenv())

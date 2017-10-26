#' plain_tweets
#'
#' Clean up character vector (tweets) to more of a plain text.
#'
#' @param x The desired character vector or data frame/list with named column/element
#'   "text" to be cleaned and processed.
#' @return Data reformatted with ascii encoding and normal ampersands and
#'   without URL links, linebreaks, fancy spaces/tabs, fancy apostrophes,
#' @export
plain_tweets <- function(x) UseMethod("plain_tweets")

#' @export
plain_tweets.default <- function(x) x

#' @export
plain_tweets.data.frame <- function(x) {
  if (has_name_(x, "text")) {
    stopifnot(is.character(x[["text"]]))
    x[["text"]] <- plain_tweets(x[["text"]])
  } else {
    stop("Couldn't find \"text\" variable.", call. = FALSE)
  }
  x
}

#' @export
plain_tweets.list <- function(x) {
  if (has_name_(x, "text")) {
    stopifnot(is.character(x[["text"]]))
    x[["text"]] <- plain_tweets(x[["text"]])
  } else {
    stop("Couldn't find \"text\" variable.", call. = FALSE)
  }
  x
}

#' @export
plain_tweets.character <- function(x) {
  x <- rm_links(x)
  x <- rm_linebreaks(x)
  x <- rm_fancy_apostrophes(rm_fancy_spaces(x))
  x <- rm_amp(x)
  x <- enc2ascii(x)
  trim_ws(x)
}

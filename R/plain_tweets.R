#' Clean up character vector (tweets) to more of a plain text.
#'
#' @param x The desired character vector or data frame/list with named column/element
#'   "text" to be cleaned and processed.
#' @return Data reformatted with ascii encoding and normal ampersands and
#'   without URL links, line breaks, fancy spaces/tabs, fancy apostrophes,
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
  x <- rm_fancy_spaces(x)
  x <- rm_fancy_apostrophes(x)
  x <- rm_amp(x)
  x <- enc2ascii(x)
  trim_ws(x)
}


##----------------------------------------------------------------------------##
##                  remove/replace tricky chars and URL links                 ##
##----------------------------------------------------------------------------##

rm_fancy_apostrophes <- function(x) gsub(intToUtf8(8217), "'", x)

rm_fancy_spaces <- function(x) {
  gsub("\\t", " ", gsub(intToUtf8(65039), " ", x))
}

rm_links <- function(x) {
  x <- gsub("\\s{0,1}http\\S{1,}\\s{0,1}", "", x)
  gsub("\\s{0,1}\\S{1,}(\\.com|\\.net|\\.gov|\\.io|\\.org)\\b\\s{0,1}", "", x)
}

rm_linebreaks <- function(x, y = " ") {
  gsub("\\n", y, x)
}

enc2ascii <- function(x, y = "") {
  iconv(x, to = "ascii", sub = y)
}

rm_amp <- function(x, y = "&") {
  if (is.null(y)) {
    y <- ""
  }
  gsub("&amp;", y, x)
}

trim_ws <- function(x) {
  x <- gsub("\\s{2,}", " ", x)
  gsub("^\\s|\\s$", "", x)
}

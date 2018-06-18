#' Clean up character vector (tweets) to more of a plain text.
#'
#' @param x The desired character vector or data frame/list with named column/element
#'   "text" to be cleaned and processed.
#' @return Data reformatted with ascii encoding and normal ampersands and
#'   without URL links, line breaks, fancy spaces/tabs, fancy apostrophes,
#' @export
plain_tweets <- function(x) {
  if (is.data.frame(x)) {
    if (has_name_(x, "text")) {
      x$text <- plain_tweets_(x$text)
    } else {
      stop("Couldn't find \"text\" variable.", call. = FALSE)
    }
  } else if (is.list(x)) {
    if (has_name_(x, "text")) {
      x$text <- plain_tweets_(x$text)
    } else {
      stop("Couldn't find \"text\" variable.", call. = FALSE)
    }
  } else {
    x <- plain_tweets_(x)
  }
  x
}

plain_tweets_ <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  stopifnot(is.character(x))
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
  x <- gsub("\\s?https?[[:graph:]]", "", x)
  gsub("\\s?\\b[[:graph:]]+(\\.com|\\.net|\\.gov|\\.io|\\.org)\\b", "", x)
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
  x <- gsub("[ ]{2,}", " ", x)
  gsub("^[ ]+|[ ]+$", "", x)
}

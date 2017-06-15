
#' plain text tweets
#'
#' @param txt Tweet text, character vector.
#' @param tokenize Logical indicating whether to split each tweet into
#'   a vector of individual words. Defaults to FALSE.
#' @param hashtags Logical indicating whether to treat hashtags as
#'   normal text. Defaults to false.
#' @param mentions Logical indicating whether to treat user mentions
#'   as normal text. Defaults to false.
#' @param links Logical indicating whether to treat links as normal
#'   text. Defaults to false.
#' @return Stripped and cleaned up version of tweet text.
#' @export
plain_tweets <- function(txt, tokenize = FALSE,
                         hashtags = FALSE,
                         mentions = FALSE,
                         links = FALSE) {
    stopifnot(is.character(txt))
    txt <- tolower(txt)
    if (!links) {
        txt <- gsub("(http|www\\.)\\S{1,}", "", txt, perl = TRUE)
        txt <- gsub("[[:alnum:]]{1,}\\.(com|edu|net|org)\\S{0,}", "", txt)
    }
    if (!mentions) {
        txt <- gsub("@\\S{1,}", "", txt, perl = TRUE)
    }
    if (!hashtags) {
        txt <- gsub("#\\S{1,}", "", txt, perl = TRUE)
    }
    txt <- gsub("^rt ", "", txt)
    txt <- gsub("amp;", " ", txt)
    txt <- gsub("\u0027|\u2019|\u2018", "", txt)
    txt <- gsub("\\'|\\-", "", txt)
    txt <- gsub("[[:punct:]]", " ", txt)
    txt <- gsub("[a-z]{0,}[[:digit:]]{1,}[a-z]{0,}", "", txt)
    txt <- gsub("[^\\w]", " ", txt, perl = TRUE)
    txt <- gsub("\\s{2,}", " ", txt, perl = TRUE)
    txt <- trimws(txt)
    if (tokenize) {
        strsplit(txt, " ")
    } else {
        txt
    }
}

word_freq <- function(txt) {
  if (all(is.data.frame(txt), isTRUE("text" %in% names(txt)))) {
    txt <- txt[["text"]]
  }
  txt <- plain_tweets(txt)
  txt <- unlist(strsplit(txt, " "))
  txt <- txt[!txt %in% c(" ", "")]
  txt <- table(txt)
  txt <- data.frame(
    word = names(txt),
    n = as.integer(txt),
    stringsAsFactors = FALSE
  )
  if (requireNamespace("tibble", quietly = TRUE)) {
    txt <- tibble::as_tibble(txt)
  }
  txt
}

#' n words
#'
#' @param data Data frame.
#' @param group Group variable.
#' @return Data frame
#' @export
n_words <- function(data, group = NULL) {
  if (inherits(data, "grouped_df")) {
    group <- names(attr(data, "labels"))
    data <- data.frame(data)
  }
  if (!is.null(group)) {
    groupfun(data, group, word_freq)
  } else {
    word_freq(data)
  }
}

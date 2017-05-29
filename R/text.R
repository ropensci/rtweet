
#' plain text tweets
#'
#' @param txt Tweet text, character vector.
#' @param tokenize Logical indicating whether to split each tweet into
#'   a vector of individual words. Defaults to FALSE.
#' @param include_hashtags Logical indicating whether to treat hashtags as
#'   normal text. Defaults to false.
#' @param include_mentions Logical indicating whether to treat user mentions
#'   as normal text. Defaults to false.
#' @param include_links Logical indicating whether to treat links as normal
#'   text. Defaults to false.
#' @return Stripped and cleaned up version of tweet text.
#' @export
plain_tweets <- function(txt, tokenize = FALSE,
                         include_links = FALSE,
                         include_mentions = FALSE,
                         include_hashtags = FALSE) {
    stopifnot(is.character(txt))
    txt <- tolower(txt)
    if (!include_links) {
        txt <- gsub("(http|www\\.)\\S{1,}", "", txt, perl = TRUE)
        txt <- gsub("[[:alnum:]]{1,}\\.(com|edu|net|org)\\S{0,}", "", txt)
    }
    if (!include_mentions) {
        txt <- gsub("@\\S{1,}", "", txt, perl = TRUE)
    }
    if (!include_hashtags) {
        txt <- gsub("#\\S{1,}", "", txt, perl = TRUE)
    }
    txt <- gsub("^rt ", "", txt)
    txt <- gsub("amp;", "", txt)
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


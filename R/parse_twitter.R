parse_twitter <- function(.x) {
  UseMethod("parse_twitter")
}

is_file <- function(x) {
  is.character(x) && length(x) == 1 &&
    !grepl("\\n", x) && file.exists(x)
}

parse_twitter.list <- function(.x) {
  tweets_with_users(.x)
}

parse_twitter.data.frame <- function(.x) {
  .x <- list(.x)
  parse_twitter(.x)
}

parse_twitter.stream_file <- function(.x) {
  parse_stream(.x)
}

parse_twitter.connection <- function(.x) {
  .x <- summary(.x)$description
  parse_stream(.x)
}


parse_twitter.character <- function(.x) {
  if (is_file(.x)) {
    class(.x) <- c("stream_file", "character")
  } else if (length(.x) > 1) {
    .x <- paste0("[", .x, "]")
    .x <- .parse_stream_two(.x)
    return(.x)
  } else {
    .x <- parse_stream(.x)
    return(.x)
  }
  parse_twitter(.x)
}




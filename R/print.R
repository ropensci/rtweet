#' @export
print.post_tweet <- function(x, ...) {
  if (httr::status_code(x) != 200L) {
    cat("Your message has not been posted!", call. = FALSE)
  }
  ct <- httr::content(x)
  cat("Tweet", ct$id_str, "posted from", ct$user$screen_name,  "\nText:", ct$text)
}

#' @export
print.tweet_deleted <- function(x, ...) {
  if (httr::status_code(x) != 200L) {
    cat("Your message has not been deleted!", call. = FALSE)
  }
  ct <- httr::content(x)
  created <- format_date(ct$created_at)
  cat("Tweet", ct$id_str, "deleted from", ct$user$screen_name,
      "\nCreated at", as.character(created), ":", ct$text)
}

#' @export
print.tweets <- function(x, ...) {
  cat("Users data at users_data()\n")
  NextMethod(x)
}

#' @export
print.users <- function(x, ...) {
  cat("Tweets data at tweets_data()")
  NextMethod(x)
}

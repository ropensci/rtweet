#' Parsing data into tweets/users data tibbles
#'
#' For internal use only
#'
#' @param x A list of responses, with one element for each page.
#' @return A tweets/users tibble with users/tweets attribute.
#' @export
tweets_with_users <- function(x) {
  empty_response <- vapply(x, is.null, logical(1L))
  x <- x[!empty_response]

  if (length(x) == 0) {
    tweets <- tweet(NULL)[0, ]
  } else {
    tweets <- do.call(rbind, lapply(x, tweet))
  }

  if (has_name_(tweets, "user") && length(tweets$user) != 0) {
    users <- do.call(rbind, tweets[["user"]])
    tweets <- tweets[!colnames(tweets) %in% "user"]
  } else {
    users <- user(NULL)[0, ]
    users <- users
  }
  users <- as_tbl(users)[, order(colnames(users))]
  tweets <- as_tbl(tweets)[, order(colnames(tweets))]

  out <- structure(tweets, users = users)
  class(out) <- c("tweets", class(out))
  out
}

#' @rdname tweets_with_users
#' @export
users_with_tweets <- function(x) {
  empty_response <- vapply(x, is.null, logical(1L))
  x <- x[!empty_response]

  if (length(x) == 0) {
    users <- user(NULL)[0, ]
  } else {
    users <- do.call(rbind, lapply(x, user))
  }

  if (length(x) == 0) {
    tweets <- tweet(NULL)[0, ]
  } else {
    status <- lapply(x, `[[`, i = "status")
    tweets <- do.call(rbind, lapply(status, tweet))
  }

  users <- as_tbl(users)[, order(colnames(users))]
  tweets <- as_tbl(tweets)[, order(colnames(tweets))]
  out <- structure(users, tweets = tweets)
  class(out) <- c("users", class(out))
  out
}


as_tbl <- function(x, ...) {
  tibble::as_tibble(x, ...)
}

clean_source <- function(s) {
  sub("<\\/.*", "", sub("[^>]+>", "", s))
}

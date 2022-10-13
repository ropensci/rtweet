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

  tweets <- tweet(NULL)[0, ]
  if (length(x) != 0) {
    tweets <- do.call(rbind, lapply(x, tweet))[, colnames(tweets)]
  }

  users <- user(NULL)[0, ]
  if (has_name_(tweets, "user") && length(tweets$user) != 0 && all(lengths(tweets$user) != 0)) {
    users <- do.call(rbind, tweets[["user"]])[, order(colnames(users))]
  }
  tweets <- tweets[!colnames(tweets) %in% "user"]
  users <- as_tbl(users)
  tweets <- as_tbl(tweets)

  out <- structure(tweets, users = users)
  class(out) <- c("tweets", class(out))
  out
}

#' @rdname tweets_with_users
#' @export
users_with_tweets <- function(x) {
  empty_response <- vapply(x, is.null, logical(1L))
  x <- x[!empty_response]

  users <- user(NULL)[0, ]
  if (length(x) != 0) {
    users <- do.call(rbind, lapply(x, user))[, colnames(users)]
  }

  tweets <- tweet(NULL)[0, ]
  if (length(x) != 0) {
    status <- lapply(x, `[[`, i = "status")
    tweets <- do.call(rbind, lapply(status, tweet))[, colnames(tweets)]
  }
  tweets <- tweets[!colnames(tweets) %in% "user"]
  users <- as_tbl(users)
  tweets <- as_tbl(tweets)
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

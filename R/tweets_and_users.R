#' Parsing data into tweets/users data tibbles
#' 
#' For internal use only
#' 
#' @param x A list of responses, with one element for each page. 
#' @return A tweets/users tibble with users/tweets attribute.
#' @keywords internal
#' @export
tweets_with_users <- function(x) {
  tweets <- do.call("rbind", lapply(x, tweet))
  if (has_name_(tweets, "user")) {
    users <- do.call("rbind", tweets[["user"]])
    tweets <- tweets[!colnames(tweets) %in% "user"]
  } else {
    users <- user(NULL)
  }
  if (lengths(x)[1] == 0) {
    tweets <- tweets[0, ]
    users <- users[0, ]
  }
  structure(tweets, users = users)
}

#' @rdname tweets_with_users
#' @export
users_with_tweets <- function(x) {
  users <- do.call("rbind", lapply(x, user))
  status <- lapply(x, `[[`, i = "status")
  tweets <- do.call("rbind", lapply(status, tweet))
  
  if (lengths(x)[1] == 0) {
    tweets <- tweets[0, ]
    users <- users[0, ]
  }
  structure(users, tweets = tweets)
}


as_tbl <- function(x, ...) {
  tibble::as_tibble(x, ...)
}

clean_source <- function(s) {
  sub("<\\/.*", "", sub("[^>]+>", "", s))
}

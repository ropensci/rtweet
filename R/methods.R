# rbind ####
#' @export
rbind.tweets_with_users <- function(..., deparse.level = 1) {
  if (...length() == 1) return(..1)
  rt <- list(...)

  ud <- lapply(rt, users_data)
  udm <- do.call("rbind", ud)
  rt <- do.call("rbind.data.frame", rt)
  rt <- as_tbl(rt)
  attr(rt, "users") <- as_tbl(udm)
  class(rt) <- c("tweets_with_users", class(rt))
  rt
}

#' @export
rbind.users_with_tweets <- function(..., deparse.level = 1) {
  if (...length() == 1) return(..1)
  rt <- list(...)

  td <- lapply(rt, tweets_data)
  tdm <- do.call("rbind", td)
  rt <- do.call("rbind.data.frame", rt)
  rt <- as_tbl(rt)
  attr(rt, "tweets") <- as_tbl(tdm)
  class(rt) <- c("users_with_tweets", class(rt))
  rt
}


# subset ####
#' @export
`[.tweets_with_users` <- function(x, i, j, ..., drop = TRUE) {
  tweets <- NextMethod()
  users <- users_data(x)[i, ]
  attr(tweets, "users") <- users
  tweets
}

#' @export
`[.users_with_tweets` <- function(x, i, j, ..., drop = TRUE) {
  users <- NextMethod()
  tweets <- tweets_data(x)[i, ]
  attr(users, "tweets") <- tweets
  users
}

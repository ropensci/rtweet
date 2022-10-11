# rbind ####
#' @export
rbind.tweets <- function(..., deparse.level = 1) {
  if (...length() == 1) return(..1)
  rt <- list(...)

  ud <- lapply(rt, users_data)
  udm <- do.call(rbind, ud)
  rt <- do.call(rbind.data.frame, rt)
  rt <- as_tbl(rt)
  attr(rt, "users") <- as_tbl(udm)
  class(rt) <- c("tweets", class(rt))
  rt
}

#' @export
rbind.users <- function(..., deparse.level = 1) {
  if (...length() == 1) return(..1)
  rt <- list(...)

  td <- lapply(rt, tweets_data)
  tdm <- do.call(rbind, td)
  rt <- do.call(rbind.data.frame, rt)
  rt <- as_tbl(rt)
  attr(rt, "tweets") <- as_tbl(tdm)
  class(rt) <- c("users", class(rt))
  rt
}


# subset ####
#' @export
`[.tweets` <- function(x, i, j, ..., drop) {
  tweets <- NextMethod()
  keep_users <- missing(drop) || !drop
  if (keep_users) {
    users <- users_data(x)[i, ]
    attr(tweets, "users") <- users
  }
  tweets
}

#' @export
`[.users` <- function(x, i, j, ..., drop) {
  users <- NextMethod()
  keep_tweets <- missing(drop) || !drop
  if (keep_tweets) {
    tweets <- tweets_data(x)[i, ]
    attr(users, "tweets") <- tweets
  }
  users
}

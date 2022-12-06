# rbind ####
#' @export
rbind.tweets <- function(..., deparse.level = 1) {
  if (...length() == 1) return(..1)
  rt <- list(...)

  ud <- lapply(rt, users_data)
  udm <- do.call(rbind, ud)
  rt <- do.call(rbind.data.frame, rt)
  rt <- tibble::as_tibble(rt)
  attr(rt, "users") <- tibble::as_tibble(udm)
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
  rt <- tibble::as_tibble(rt)
  attr(rt, "tweets") <- tibble::as_tibble(tdm)
  class(rt) <- c("users", class(rt))
  rt
}


# subset ####
#' @export
`[.tweets` <- function(x, i, j, ..., drop) {
  tweets <- NextMethod()
  keep_users <- missing(drop) || !drop
  if (keep_users) {
    attr(tweets, "users") <- users_data(x)[i, ]
  }
  tweets
}

#' @export
`[.users` <- function(x, i, j, ..., drop) {
  users <- NextMethod()
  keep_tweets <- missing(drop) || !drop
  if (keep_tweets) {
    attr(users, "tweets") <- tweets_data(x)[i, ]
  }
  users
}

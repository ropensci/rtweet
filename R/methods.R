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
#' @export
rbind.page <- function(..., deparse.level = 1) {
  if (...length() == 1) return(..1)
  rt <- list(...)

  tdm <- do.call(rbind.data.frame, rt)

  m <- lapply(rt, attr, which = "meta", exact  = TRUE)
  coln <- lapply(m, colnames)
  coln <- unique(unlist(coln, FALSE, FALSE))
  mm <- matrix(nrow = length(m), ncol = length(coln))
  colnames(mm) <- coln
  mm <- as.data.frame(mm)
  for (i in seq_along(m)) {
    mm[i, colnames(m[[i]])] <- m[[i]][1, ]
  }
  attr(tdm, "meta") <- as.data.frame(mm)

  m <- lapply(rt, attr, which = "summary", exact  = TRUE)
  coln <- lapply(m, colnames)
  coln <- unique(unlist(coln, FALSE, FALSE))
  mm <- matrix(nrow = length(m), ncol = length(coln))
  colnames(mm) <- coln
  mm <- as.data.frame(mm)
  for (i in seq_along(m)) {
    mm[i, colnames(m[[i]])] <- m[[i]][1, ]
  }
  attr(tdm, "summary") <- as.data.frame(mm)

  class(tdm) <- c("result", class(tdm))
  return(tdm)
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

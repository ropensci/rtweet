#' Search tweets (vectorized)
#'
#' Returns data from one or more search queries.
#' 
#' @param q Vector of search queries.
#' @param n Number of tweets to return per query. Defaults to 100.
#'   Must be of length 1 or equal to length of user.
#' @param \dots Other arguments passed on to \code{search_tweets}.
#' @return A tbl data frame with additional "query" feature.
#' @export
search_tweets_ <- function(q, n = 100, ...) {
  ## check inputs
  stopifnot(is.atomic(q), is.numeric(n))
  if (length(q) == 0L) {
    stop("No query found", call. = FALSE)
  }
  ## if missing issue warning and save NA position
  if (any(is.na(q))) {
    warning("NA queries found. Empty data frames returned for missing q values", call. = FALSE)
    mia <- which(is.na(q))
    q[is.na(q)] <- ""
  }
  ## search for each string in column of queries
  rt <- Map(search_tweets, q, n = n, ...)
  ## if missing, label appropriately
  if (any(is.na(q))) {
    q[mia] <- NA_character_
  }
  kp <- vapply(rt, function(x) NROW(x) > 0L, logical(1))
  if (sum(kp, na.rm = TRUE) == 0L) return(data.frame())
  rt <- rt[kp]
  q <- q[kp]
  ## add query variable to data frames
  rt <- Map(cbind, rt, query = q, stringsAsFactors = FALSE)
  ## merge users data into one data frame
  rt_users <- do.call("rbind", lapply(rt, users_data))
  ## merge tweets data into one data frame
  rt <- do.call("rbind", rt)
  ## set users attribute
  attr(rt, "users") <- rt_users
  ## return tibble (validate = FALSE makes it a bit faster)
  tibble::as_tibble(rt, validate = FALSE)
}


#' Get timeline (vectorized)
#'
#' Returns one or more user timelines.
#' 
#' @param user Vector of user names, user IDs, or a mixture of both.
#' @param n Number of tweets to return per timeline. Defaults to 100.
#'   Must be of length 1 or equal to length of user.
#' @param \dots Other arguments passed on to \code{get_timeline}.
#' @return A tbl data frame of tweets data with users data attribute.
#' @export
get_timeline.default <- function(user, n = 100, ...) {
  ## check inputs
  stopifnot(is.atomic(user), is.numeric(n))
  if (length(user) == 0L) {
    stop("No query found", call. = FALSE)
  }
  ## search for each string in column of queries
  rt <- Map(get_timeline_, user, n = n, ...)
  ## merge users data into one data frame
  rt_users <- do.call("rbind", lapply(rt, users_data))
  ## merge tweets data into one data frame
  rt <- do.call("rbind", rt)
  ## set users attribute
  attr(rt, "users") <- rt_users
  ## return tibble (validate = FALSE makes it a bit faster)
  tibble::as_tibble(rt, validate = FALSE)
}

#' Get timeline
#'
#' Returns one or more user timelines (tweets posted by target user(s))
#' 
#' @param user Vector of user names, user IDs, or a mixture of both.
#' @param n Number of tweets to return per timeline. Defaults to 100.
#'   Must be of length 1 or equal to length of user.
#' @param \dots Other arguments passed on to \code{get_timeline}.
#' @return A tbl data frame of tweets data with users data attribute.
#' @export
get_timeline <- function(user, n, ...) {
  UseMethod("get_timeline")
}






#' Get friends (vectorized)
#'
#' Returns friends for one or more users
#'
#' @param user Vector of user names, user IDs, or both.
#' @param \dots Other arguments passed along to \code{get_friends}
#' @return A tbl data frame of user and friends (user_id column)
#' @export
get_friends_ <- function(user, ...) {
  ## check inputs
  stopifnot(is.atomic(user))
  if (length(user) == 0L) {
    stop("No query found", call. = FALSE)
  }
  ## search for each string in column of queries
  rt <- lapply(user, get_friends, ...)
  rt <- Map(cbind, user = user, rt, stringsAsFactors = FALSE)
  ## merge data into one data frame
  rt <- do.call("rbind", rt)
  ## return tibble (validate = FALSE makes it a bit faster)
  tibble::as_tibble(rt, validate = FALSE)
}


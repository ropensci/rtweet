#' tweets_df
#'
#' @description Converts tweets object (nested list converted from
#'   json object) into a [tibble] data frame.
#'
#' @param dat Tweets object or nested list. Usually this is the
#'   return object produced by \code{\link{search_tweets}} or
#'   \code{\link{stream_tweets}}.
#'
#' @importFrom dplyr bind_cols
#' @export
tweets_df <- function(dat) {

  if (missing(dat)) {
    stop("Must specify tweets object, dat.", call. = TRUE)
  }

  if ("statuses" %in% names(dat)) {
    dat <- dat[["statuses"]]
  }

  if ("status" %in% names(dat)) {
    dat <- dat[["status"]]
  }

  tweets_df <- bind_cols(
    tweets_toplevel_df(dat),
    tweets_entities_df(dat),
    tweets_retweet_df(dat),
    tweets_place_df(dat))

  tweets_df
}

#' user_df
#'
#' @description Converts user object (nested list converted from
#'   json object) into a [tibble] data frame.
#'
#' @param dat User object or nested list. Usually this is the
#'   return object produced by \code{\link{lookup_users}}.
#'
#' @importFrom dplyr bind_cols
#' @export
user_df <- function(dat) {

  if ("user" %in% names(dat)) {
    dat <- dat[["user"]]
  }

  user_df <- bind_cols(
    user_toplevel_df(dat),
    user_entities_df(dat))

  user_df
}

#' parser
#'
#' @description Parses tweets and users objects. Returns data frames
#'   for each.
#'
#' @param x List, fromJSON nested list object
#' @param n Numeric, number of desired tweets to return
#'
#' @importFrom dplyr bind_rows
#' @noRd
parser <- function(x, n = NULL) {
  if (is.data.frame(x)) {
    tweets <- parse_tweets(x)
    users <- parse_users(x)
  } else {
    stopifnot(is.list(x))

    tweets <- bply(x, parse_tweets)
    tweets <- n_rows(tweets, n)

    users <- bply(x, parse_users)
    users <- n_rows(users, n)
  }

  list(
    tweets = tweets,
    users = users)
}

#' parse_fs
#'
#' @description Returns next_cursor and tibble data frame of user
#'   ids.
#'
#' @param x List, nested fromJSON object.
#' @param n Numeric, number of desired ids to return.
#' @return List of length 2, containing next_cursor (to be used
#'   with \code{page} argument in loops to collect maxiimum ids)
#'   and ids data frame (of followers or friends depending on
#'   parent function used to generate x input).
#'
#' @noRd
#' @importFrom dplyr tbl_df
parse_fs <- function(x, n = NULL) {
  if (length(x) == 1) {
    next_cursor <- x[[1]][["next_cursor_str"]]
    x <- x[[1]][["ids"]]
  } else if (length(x) > 1) {
    next_cursor <- unlist(lapply(x, function(x) x[["next_cursor_str"]]))
    next_cursor <- return_last(next_cursor)
    x <- unlist(lapply(x, function(x) x[["ids"]]))
  }
  if (!is.null(n)) {
    if (n < length(x)) {
      x <- x[seq_along(n)]
    }
  }
  list(next_cursor = next_cursor, ids = tbl_df(x))
}

parse_tweets <- function(x) {

  if ("statuses" %in% names(x)) {
    x <- x[["statuses"]]
  } else if ("status" %in% names(x)) {
    x <- x[["status"]]
  }

  if (!"friends_count" %in% names(x)) {
    return(tweets_df(x))
  }

  return(invisible())
}

check_user_obj <- function(x) {

  if ("user" %in% names(x)) {
    x <- x[["user"]]
  }

  if (!"id_str" %in% names(x)) {
    if ("id" %in% names(x)) {
      x$id_str <- x$id
    } else {
      stop("object does not contain ID variable.", call. = FALSE)
    }
  }
  x
}

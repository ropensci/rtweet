


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

  tweets_df[!duplicated(tweets_df), ]
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

  user_df <- user_df[!duplicated(user_df), ]
}

#' rtweet_parser
#'
#' @description Parses tweets and users objects. Returns data frames
#'   for each.
#'
#' @param x List, fromJSON nested list object
#' @param n Numeric, number of rows to return for tweets object
#'
#' @return Parsed data frame
#' @export
rtweet_parser <- function(x, n = NULL) {
  parser(x, n)
}

#' @importFrom dplyr bind_rows
#' @keywords internal
#' @export
parser <- function(x, n = NULL) {
  tweets <- data.frame()
  users <- data.frame()

  if (all(is.data.frame(x), "id_str" %in% names(x))) {
    tweets <- parse_tweets(x)
    users <- parse_users(x)
  } else {
    stopifnot(is.list(x))
    tweets <- bply(x, parse_tweets)
    users <- bply(x, parse_users)
  }

  tweets <- return_n_rows(tweets, n)
  users <- return_n_rows(users, n)
  users <- filter_na_rows(users)

  list(tweets = tweets, users = users)
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
parse_fs <- function(x, n = NULL) {
  if (length(x) == 1) {
    next_cursor <- x[[1]][["next_cursor_str"]]
    x <- x[[1]][["ids"]]
  } else if (length(x) > 1) {
    next_cursor <- unlist(lapply(x, function(x) x[["next_cursor_str"]]))
    next_cursor <- return_last(next_cursor)
    x <- unlist(lapply(x, function(x) x[["ids"]]))
  }
  x <- return_n_rows(x, n)

  names(x) <- "ids"

  attr(x, "next_cursor") <- next_cursor

  x
}

#' parse_tweets
#'
#' @description Converts nested json list object to tweets
#'   data_frame
#' @param x Nested json list object
#'
#' @return Tweets data as tbl (tibble) data table
#' @export
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

#' parse_users
#'
#' @description Converts nested json list object to users
#'   data_frame
#' @param x Nested json list object
#'
#' @return Users data as tbl (tibble) data table
#' @export
parse_users <- function(x) {

  if ("friends_count" %in% names(x)) {
    return(user_df(x))
  }

  if ("statuses" %in% names(x)) {
    x <- x[["statuses"]]
  }

  if ("user" %in% names(x)) {
    return(user_df(x[["user"]]))
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

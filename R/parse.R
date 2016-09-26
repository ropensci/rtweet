
tweets_df <- function(dat, clean_tweets = TRUE) {

  if (missing(dat)) {
    stop("Must specify tweets object, dat.", call. = TRUE)
  }

  if ("statuses" %in% names(dat)) {
    dat <- dat[["statuses"]]
  }

  if ("status" %in% names(dat)) {
    dat <- dat[["status"]]
  }

  tweets_df <- cbind_(
    tweets_toplevel_df(dat),
    tweets_entities_df(dat),
    tweets_retweet_df(dat),
    tweets_place_df(dat))

  if (clean_tweets) {
    tweets_df[["text"]] <- clean_tweets(tweets_df[["text"]])
  }
  unique(tweets_df)
}

#' clean_tweets
#'
#' @description Converts tweets to to ASCII
#' @param x Twitter text
#'
#' @export
clean_tweets <- function(x) {
  iconv(x, "UTF-8", "ASCII", "")
}

#' utf8_tweets
#'
#' @description Converts tweets to UTF-8 encoding
#' @param x Twitter text
#'
#' @export
utf8_tweets <- function(x) {
	unlist(lapply(x, function(.) if (Encoding(.) != "UTF-8") enc2utf8(x)),
		recursive = FALSE)
}


user_df <- function(dat) {

  if ("user" %in% names(dat)) {
    dat <- dat[["user"]]
  }

  user_df <- cbind_(
    user_toplevel_df(dat),
    user_entities_df(dat))

  unique(user_df)
}

#' parser
#'
#' @param x nested list of API data returned from fromJSON
#' @param n desired number to return
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
  tweets <- tweets[!is.na(tweets$status_id), ]
  tweets <- unique(tweets)
  users <- return_n_rows(users, n)
  if (is.data.frame(users)) {
  	users <- filter_na_rows(users)
  }
  list(tweets = tweets, users = users)
}


parse_fs <- function(x, n = NULL) {
  if (length(x) == 1) {
    next_cursor <- x[[1]][["next_cursor_str"]]
    x <- as.double(x[[1]][["ids"]])
  } else if (length(x) > 1) {
    next_cursor <- unlist(lapply(x, function(x) x[["next_cursor_str"]]))
    next_cursor <- return_last(next_cursor)
    x <- unlist(lapply(x, function(x) x[["ids"]]))
  }

  x <- return_n_rows(x, n)
  x <- data_frame_(x)
  names(x) <- "ids"

  attr(x, "next_cursor") <- next_cursor
  x
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

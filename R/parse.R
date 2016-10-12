
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

  tweets_df <- cbind(
    tweets_toplevel_df(dat),
    tweets_entities_df(dat),
    tweets_retweet_df(dat),
    tweets_place_df(dat))

  if (clean_tweets) {
    tweets_df[["text"]] <- clean_tweets(tweets_df[["text"]])
  }
  tweets_df[!duplicated(tweets_df[, c(2, 13)]), ]
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
		recursive = FALSE, use.names = FALSE)
}


user_df <- function(dat) {

  if ("user" %in% names(dat)) {
    dat <- dat[["user"]]
  }

  user_df <- cbind(
    user_toplevel_df(dat),
    user_entities_df(dat))

  unique(user_df)
}

#' parser
#'
#' @param x nested list of API data returned from fromJSON
#' @param n desired number to return
#' @param return_tweets logical indicating whether to return tweets data
#'   object.
#' @param return_users logical indicating whether to return users data
#'   object.
#' @keywords internal
#' @export
parser <- function(x, n = NULL, return_tweets = TRUE, return_users = TRUE) {
  tweets <- data.frame()
  users <- data.frame()

  if (all(is.data.frame(x), isTRUE("id_str" %in% names(x)))) {
    if (return_tweets) {
      tweets <- parse_tweets(x)
    }
    if (return_users) {
      users <- parse_users(x)
    }
  } else {
    stopifnot(is.list(x))
    if (return_tweets) {
      tweets <- bply(x, parse_tweets)
    }
    if (return_users) {
      users <- bply(x, parse_users)
    }
  }
  if (return_tweets) {
    tweets <- return_n_rows(tweets, n)
    if ("status_id" %in% names(tweets)) {
      if (!is.null(tweets$status_id)) {
        tweets <- tweets[!is.na(tweets$status_id), ]
      }
    }
  }
  if (return_users) {
    users <- return_n_rows(users, n)
    if (is.data.frame(users)) {
      users <- filter_na_rows(users)
      users <- unique(users)
    }
  }
  list(tweets = tweets, users = users)
}

parse_fs <- function(x, n = NULL) {
	if (identical(length(x), 1)) {
		next_cursor <- x[[1]][["next_cursor_str"]]
		x <- as.double(x[[1]][["ids"]])
	} else if (all(c("ids", "next_cursor_str") %in% names(x))) {
		next_cursor <- x[["next_cursor_str"]]
		x <- as.double(x[["ids"]])
	} else if (length(x) > 1) {
		next_cursor <- unlist(lapply(x, function(x) x[["next_cursor_str"]]),
			use.names = FALSE)
		next_cursor <- return_last(next_cursor)
		x <- unlist(lapply(x, function(x) x[["ids"]]), use.names = FALSE)
	}

	x <- return_n_rows(x, n)
	x <- data.frame(x, stringsAsFactors = FALSE)
	names(x) <- "ids"

	attr(x, "next_cursor") <- next_cursor
	x
}

parse_fs2 <- function(x, n = NULL) {
	x <- rawToChar(x$content)
	if (grepl("errors", x)) {
		x <- NA_real_
		next_cursor <- NULL
	} else {
		x <- strsplit(x, "\\]|\\[")[[1]]
		if (length(x) > 2) {
			next_cursor <- strsplit(x[3], "\\,|\\:")[[1]][[3]]
		} else {
			next_cursor <- NULL
		}
		x <- as.double(strsplit(x[2], ",")[[1]])
	}
	x <- data.frame(x, stringsAsFactors = FALSE)
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

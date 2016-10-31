
tweets_df <- function(dat, as_double = FALSE, clean_tweets = FALSE) {

  if (missing(dat)) {
    stop("Must specify tweets object, dat.", call. = TRUE)
  }

  if ("statuses" %in% names(dat)) {
    dat <- dat[["statuses"]]
  }
  lookup <- FALSE
  if ("status" %in% names(dat)) {
    lookup <- TRUE
    screen_name <- dat[["screen_name"]]
    user_id <- dat[["id_str"]]
    dat <- dat[["status"]]
  }

  tweets_df <- cbind(
    tweets_toplevel_df(dat, as_double = as_double),
    tweets_entities_df(dat),
    tweets_retweet_df(dat),
    tweets_place_df(dat))

  if (clean_tweets) {
    tweets_df[["text"]] <- cleantweets(tweets_df[["text"]])
  }
  if (lookup) {
    tweets_df[["screen_name"]] <- screen_name
    tweets_df[["user_id"]] <- user_id
  }
  tweets_df <- tweets_df[row.names(unique(tweets_df[, 1:13])), ]
  row.names(tweets_df) <- NULL
  tweets_df
}

#' cleantweets
#'
#' @description Converts tweets to to ASCII
#' @param x Twitter text
#'
#' @export
cleantweets <- function(x) {
  iconv(x, "UTF-8", "ASCII", "")
  #iconv(x, "latin1", "ASCII", "")
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


user_df <- function(dat, as_double = FALSE) {

  if ("user" %in% names(dat)) {
    dat <- dat[["user"]]
  }

  user_df <- cbind(
    user_toplevel_df(dat, as_double = as_double),
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
#' @param clean_tweets logical indicating whether to remove non-ASCII
#'   characters in text of tweets. defaults to FALSE.
#' @param as_double logical indicating whether to handle ID variables
#'   as double (numeric) class. By default, this is set to FALSE, meaning
#'   ID variables are treated as character vectors. Setting this to
#'   TRUE can provide performance (speed and memory) boost but can also
#'   lead to issues when printing and saving, depending on the format.
#' @keywords internal
#' @export
parser <- function(x, n = NULL, return_tweets = TRUE, return_users = TRUE,
                   clean_tweets = FALSE, as_double = FALSE) {

  tweets <- data.frame()
  users <- data.frame()

  if (all(is.data.frame(x), isTRUE("id_str" %in% names(x)))) {
    if (return_tweets) {
      tweets <- parse_tweets(x, clean_tweets = clean_tweets, as_double = as_double)
    }
    if (return_users) {
      users <- parse_users(x, as_double = as_double)
    }
  } else {
    stopifnot(is.list(x))
    if (return_tweets) {
      tweets <- bply(x, parse_tweets, clean_tweets = clean_tweets, as_double = as_double)
    }
    if (return_users) {
      users <- bply(x, parse_users, as_double = as_double)
    }
  }
  if (return_tweets) {
    tweets <- return_n_rows(tweets, n)
    if ("status_id" %in% names(tweets)) {
      if (!is.null(tweets$status_id)) {
        tweets <- tweets[!is.na(tweets$status_id), ]
        tweets <- tweets[row.names(unique(tweets[, 1:13])), ]
        row.names(tweets) <- NULL
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

parse_fs <- function(x, n = NULL, as.double = FALSE) {
	if (identical(length(x), 1)) {
		next_cursor <- x[[1]][["next_cursor_str"]]
		if (as_double) {
		  x <- as.double(x[[1]][["ids"]])
		} else {
		  x <- as.character(x[[1]][["ids"]])
		}
	} else if (all(c("ids", "next_cursor_str") %in% names(x))) {
		next_cursor <- x[["next_cursor_str"]]
		if (as_double) {
		  x <- as.double(x[[1]][["ids"]])
		} else {
		  x <- as.character(x[[1]][["ids"]])
		}
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

parse_fs2 <- function(x, n = NULL, as_double = FALSE) {
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
		if (as_double) {
		  x <- as.double(x[[1]][["ids"]])
		} else {
		  x <- as.character(x[[1]][["ids"]])
		}
	}
	x <- data.frame(x, stringsAsFactors = FALSE)
	names(x) <- "ids"

	attr(x, "next_cursor") <- next_cursor
	x
}



parse_tweets <- function(x, clean_tweets = FALSE, as_double = FALSE) {
  lookup <- FALSE
  if ("statuses" %in% names(x)) {
    x <- x[["statuses"]]
  } else if ("status" %in% names(x)) {
    lookup <- TRUE
    screen_name <- x[["screen_name"]]
    user_id <- x[["id_str"]]
    x <- x[["status"]]
  }

  if (!"friends_count" %in% names(x)) {
    x <- tweets_df(x, clean_tweets = clean_tweets, as_double = as_double)
    if (lookup) {
      x[["screen_name"]] <- screen_name
      x[["user_id"]] <- user_id
    }
    return(x)
  }

  return(invisible())
}

parse_users <- function(x, as_double = FALSE) {

  if ("friends_count" %in% names(x)) {
    return(user_df(x, as_double = as_double))
  }

  if ("statuses" %in% names(x)) {
    x <- x[["statuses"]]
  }

  if ("user" %in% names(x)) {
    return(user_df(x[["user"]], as_double = as_double))
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

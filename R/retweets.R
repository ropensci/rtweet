#' Get the most recent retweets of a specific Twitter status
#'
#' Returns a collection of the 100 most recent retweets of a given
#' status.  NOTE: Twitter's API is currently limited to 100 or fewer
#' retweeters.
#'
#' @inheritParams lookup_users
#' @param status_id required The numerical ID of the desired status.
#' @param n optional Specifies the number of records to retrieve.
#'   Must be less than or equal to 100.
#' @param ... Other arguments used as parameters in the query sent to
#'   Twitter's rest API, for example, `trim_user = TRUE`.
#' @return Tweets data of the most recent retweets of a given status
#' @export
#' @family retweets
get_retweets <- function(status_id, n = 100, parse = TRUE, token = NULL, ...) {
  stopifnot(is.character(status_id), length(status_id) == 1L)
  
  query <- sprintf("/1.1/statuses/retweets/%s", status_id)
  params <- list(
    id = status_id,
    count = n,
    ...
  )
  r <- TWIT_get(token, query, params, parse = parse)
  
  if (parse) {
    r <- tweets_with_users(r)
  }
  r
}


#' Get user IDs of users who retweeted a given status.
#'
#' Returns user IDs of users who retweeted a given status. At the
#' current time, this function is limited in returning a maximum of
#' 100 users for a given status.
#'
#' @inheritParams lookup_users
#' @param status_id required The status ID of the desired status.
#' @param n Specifies the number of records to retrieve.  Best if
#'   intervals of 100.
#' @return data
#' @family retweets
#' @export
get_retweeters <- function(status_id,
                           n = 100,
                           parse = TRUE,
                           token = NULL) {
  stopifnot(is_n(n))
  
  cursor <- "-1"
  
  n <- ceiling(n / 100L)
  r <- vector("list", n)
  for (i in seq_along(r)) {
    r[[i]] <- get_retweeters_call(
      status_id = status_id, cursor = cursor, token = token
    )
    cursor <- attr(r[[i]], "next_cursor")
    if (is.null(cursor) || length(cursor) == 0 || identical(cursor, "0")) {
      break
    }
  }
  if (parse) {
    r <- do.call("rbind", r)
    attr(r, "next_cursor") <- cursor
  }
  r
}

get_retweeters_call <- function(status_id,
                                cursor = "-1",
                                parse = TRUE,
                                token = NULL) {
  stopifnot(is.character(status_id), is.character(cursor))
  
  params <- list(
    id = status_id,
    count = 100,
    cursor = cursor,
    stringify_ids = TRUE
  )
  r <- TWIT_get(token, "/1.1/statuses/retweeters/ids", params)
  
  if (parse) {
    if (has_name_(r, "next_cursor_str")) {
      next_cursor <- r$next_cursor_str
    } else {
      next_cursor <- NULL
    }
    r <- as_retweeters(r)
    r <- as.data.frame(r)
    attr(r, "next_cursor") <- next_cursor
  }
  r
}

as_retweeters <- function(x) {
  structure(x, class = "retweeters")
}

as.data.frame.retweeters <- function(x) {
  if (has_name_(x, "ids")) {
    x <- data.frame(
      user_id = x$ids,
      stringsAsFactors = FALSE
    )
  } else {
    x <- data.frame()
  }
  x
}

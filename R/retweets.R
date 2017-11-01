#' Get the most recent retweets of a specific Twitter status
#'
#' Returns a collection of the 100 most recent retweets of a given
#' status.  NOTE: Twitter's API is currently limited to 100 or fewer
#' retweeters.
#'
#' @param status_id required The numerical ID of the desired status.
#' @param n optional Specifies the number of records to retrieve.
#'   Must be less than or equal to 100.
#' @param parse Logical indicating whether to convert the response
#'   object into an R list. Defaults to TRUE.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find
#'   instructions on how to create tokens and setup an environment
#'   variable in the tokens vignette (in r, send \code{?tokens} to
#'   console).
#' @param ... Other arguments used as parameters in the query sent to
#'   Twitter's rest API, for example, \code{trim_user = TRUE}.
#' @return Tweets data of the most recent retweets of a given status
#' @details NOTE: Twitter's API is currently limited to 100 or fewer
#'   retweeters.
#' @export
#' @family retweets
get_retweets <- function(status_id, n = 100, parse = TRUE, token = NULL, ...) {
  query <- "statuses/retweets/:id"
  params <- list(
    id = status_id,
    count = n,
    ...
  )
  token <- check_token(token, query)
  url <- make_url(query = query, param = params)
  r <- httr::GET(url, token)
  if (parse) {
    r <- from_js(r)
    r <- as_retweets(r)
    r <- as.data.frame(r)
  }
  r
}

as_retweets <- function(x) {
  structure(x, class = "retweets")
}

as.data.frame.retweets <- function(x) {
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



#' Get user IDs of users who retweeted a given status.
#'
#' Returns user IDs of users who retweeted a given status. At the
#' current time, this function is limited in returning a maximum of
#' 100 users for a given status.
#'
#' @param status_id required The status ID of the desired status.
#' @param n Specifies the number of records to retrieve.  Best if
#'   intervals of 100.
#' @param parse Logical indicating whether to convert the response
#'   object into an R list. Defaults to TRUE.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find
#'   instructions on how to create tokens and setup an environment
#'   variable in the tokens vignette (in r, send \code{?tokens} to
#'   console).
#' @return data
#' @details At time of writing, pagination offers no additional
#'   data. See the post from Pipes here:
#'   \url{https://twittercommunity.com/t/paging-is-not-possible-with-statuses-retweeters-ids-json/71298/8}
#' @family retweets
#' @export
get_retweeters <- function(status_id,
                           n = 100,
                           parse = TRUE,
                           token = NULL) {
  get_retweeters_(
    status_id = status_id, n = n, parse = parse, token = token
  )
}

get_retweeters_ <- function(status_id,
                            n = 100,
                            cursor = "-1",
                            parse = TRUE,
                            token = NULL) {
  stopifnot(is_n(n))
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
  query <- "statuses/retweeters/ids"
  stopifnot(is.character(status_id), is.character(cursor))
  params <- list(
    id = status_id,
    count = 100,
    cursor = cursor,
    stringify_ids = TRUE
  )
  token <- check_token(token, query)
  url <- make_url(query = query, param = params)
  r <- httr::GET(url, token)
  if (parse) {
    r <- from_js(r)
    if (has_name_(r, "next_cursor_str")) {
      next_cursor <- r$next_cursor_str
    } else {
      next_cursor <- NULL
    }
    r <- as_retweeters(r)
    r <- as.data.frame(r)
    attr(r, "next_cursor") <- next_cursor
  } else {
    rr <- httr::content(r)
    if (has_name_(rr, "next_cursor_str")) {
      next_cursor <- rr$next_cursor_str
    } else {
      next_cursor <- NULL
    }
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

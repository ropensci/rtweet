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
#' @param token Every user should have their own Oauth (Twitter API) token. By
#'   default \code{token = NULL} this function looks for the path to a saved
#'   Twitter token via environment variables (which is what `create_token()`
#'   sets up by default during initial token creation). For instruction on how
#'   to create a Twitter token see the tokens vignette, i.e.,
#'   `vignettes("auth", "rtweet")` or see \code{?tokens}.
#' @param ... Other arguments used as parameters in the query sent to
#'   Twitter's rest API, for example, \code{trim_user = TRUE}.
#' @return Tweets data of the most recent retweets of a given status
#' @details NOTE: Twitter's API is currently limited to 100 or fewer
#'   retweeters.
#' @export
#' @family retweets
get_retweets <- function(status_id, n = 100, parse = TRUE, token = NULL, ...) {
  stopifnot(is.character(status_id), length(status_id) == 1L)
  query <- sprintf("statuses/retweets/%s", status_id)
  params <- list(
    id = status_id,
    count = n,
    ...
  )
  token <- check_token(token)
  url <- make_url(query = query, param = params)
  r <- httr::GET(url, token)
  if (parse) {
    r <- from_js(r)
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
#' @param status_id required The status ID of the desired status.
#' @param n Specifies the number of records to retrieve.  Best if
#'   intervals of 100.
#' @param parse Logical indicating whether to convert the response
#'   object into an R list. Defaults to TRUE.
#' @param token Every user should have their own Oauth (Twitter API) token. By
#'   default \code{token = NULL} this function looks for the path to a saved
#'   Twitter token via environment variables (which is what `create_token()`
#'   sets up by default during initial token creation). For instruction on how
#'   to create a Twitter token see the tokens vignette, i.e.,
#'   `vignettes("auth", "rtweet")` or see \code{?tokens}.
#' @return data
#' @details At time of writing, pagination offers no additional
#'   data.
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
  token <- check_token(token)
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

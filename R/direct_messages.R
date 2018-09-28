#' Get direct messages sent to and received by the authenticating user from the
#' past 30 days
#'
#' Returns all Direct Message events (both sent and received) within the last 30
#' days. Sorted in reverse-chronological order.
#'
#' @param n optional Specifies the number of direct messages to try
#'   and retrieve, up to a maximum of 50.
#' @param next_cursor If there are more than 200 DMs in the last 30 days,
#'   responses will include a next_cursor value, which can be supplied in
#'   additional requests to scroll through pages of results.
#' @param parse Logical indicating whether to convert response object
#'   into nested list. Defaults to true.
#' @param token Every user should have their own Oauth (Twitter API) token. By
#'   default \code{token = NULL} this function looks for the path to a saved
#'   Twitter token via environment variables (which is what `create_token()`
#'   sets up by default during initial token creation). For instruction on how
#'   to create a Twitter token see the tokens vignette, i.e.,
#'   `vignettes("auth", "rtweet")` or see \code{?tokens}.
#' @return Return parsed or non-parsed response object.
#' @examples
#'
#' \dontrun{
#'
#' ## get my direct messages
#' dms <- direct_messages()
#'
#' ## inspect data structure
#' str(dms)
#'
#' }
#'
#' @details Includes detailed information about the sender and
#'   recipient user. You can request up to 50 direct messages per
#'   call, and only direct messages from the last 30 days will be
#'   available using this endpoint.
#'
#'   Important: This method requires an access token with read,
#'   write, and direct message permissions. To change your application's
#'   permissions, navigate to \url{apps.twitter.com}, select the
#'   appropriate application, click the "permissions" tab. Once you' have made
#'   changes to the application permission settings, you will need to
#'   regenerate your token before those effect of those changes can
#'   take effect.
#' @export
direct_messages <- function(n = 50,
                            next_cursor = NULL,
                            parse = TRUE,
                            token = NULL) {
  query <- "direct_messages/events/list"
  token <- check_token(token)
  if (!is_read_write_directmessages(token)) {
    stop("Token does not have `read-write-directmessages` access level. ",
      "For DM permissions, users must create their own app at developer.twitter.com")
  }
  params <- list(count = n, next_cursor = next_cursor)
  url <- make_url(query = query, param = params)
  r <- httr::GET(url, token)
  warn_for_twitter_status(r)
  if (r$status_code == 200L && parse) {
    r <- from_js(r)
  }
  r
}


#' (DEPRECATED) Get the most recent direct messages sent to the authenticating user.
#'
#' Retrieves up to 200 of the most recently received direct messages
#' by the authenticating (home) user. This function requires access
#' token with read, write, and direct messages access.
#'
#' @param since_id optional Returns results with an ID greater than
#'   (that is, more recent than) the specified ID. There are limits to
#'   the number of Tweets which can be accessed through the API. If
#'   the limit of Tweets has occurred since the since_id, the since_id
#'   will be forced to the oldest ID available.
#' @param max_id Character, returns results with an ID less than (that is,
#'   older than) or equal to `max_id`.
#' @param n optional Specifies the number of direct messages to try
#'   and retrieve, up to a maximum of 200. The value of count is best
#'   thought of as a limit to the number of Tweets to return because
#'   suspended or deleted content is removed after the count has been
#'   applied.
#' @param parse Logical indicating whether to convert response object
#'   into nested list. Defaults to true.
#' @param token Every user should have their own Oauth (Twitter API) token. By
#'   default \code{token = NULL} this function looks for the path to a saved
#'   Twitter token via environment variables (which is what `create_token()`
#'   sets up by default during initial token creation). For instruction on how
#'   to create a Twitter token see the tokens vignette, i.e.,
#'   `vignettes("auth", "rtweet")` or see \code{?tokens}.
#' @return Return object converted to nested list. If status code of
#'   response object is not 200, the response object is returned
#'   directly.
#' @examples
#'
#' \dontrun{
#'
#' ## get my direct messages
#' dms <- direct_messages_received()
#'
#' ## inspect data structure
#' str(dms)
#'
#' ## get direct messages I've sent
#' sdms <- direct_messages_sent()
#'
#' ## inspect data structure
#' str(dms)
#'
#' }
#'
#' @details Includes detailed information about the sender and
#'   recipient user. You can request up to 200 direct messages per
#'   call, and only the most recent 200 direct messages will be available using
#'   this endpoint.
#'
#'   Important: This method requires an access token with read,
#'   write, and direct message permissions. To change your application's
#'   permissions, navigate to \url{apps.twitter.com}, select the
#'   appropriate application, click the "permissions" tab. Once you' have made
#'   changes to the application permission settings, you will need to
#'   regenerate your token before those effect of those changes can
#'   take effect.
#' @export
direct_messages_received <- function(since_id = NULL,
                                     max_id = NULL,
                                     n = 200,
                                     parse = TRUE,
                                     token = NULL) {
  stop("The endpoint for `direct_messages_received()` no longer exists. ",
    "Please use `direct_messages()` instead.")
  query <- "direct_messages"
  token <- check_token(token)
  params <- list(include_entities = TRUE, count = n,
    since_id = since_id, max_id = max_id)
  url <- make_url(query = query, param = params)
  r <- httr::GET(url, token)
  warn_for_twitter_status(r)
  if (r$status_code == 200L && parse) {
    r <- from_js(r)
  }
  r
}

#' @inheritParams direct_messages_received
#' @export
#' @rdname direct_messages
direct_messages_sent <- function(since_id = NULL,
                                 max_id = NULL,
                                 n = 200,
                                 parse = TRUE,
                                 token = NULL) {
  stop("The endpoint for `direct_messages_received()` no longer exists. ",
    "Please use `direct_messages()` instead.")
  query <- "direct_messages/events/list"
  token <- check_token(token)
  params <- list(include_entities = TRUE, count = n,
    since_id = since_id, max_id = max_id)
  url <- make_url(query = query, param = params)
  r <- httr::GET(url, token)
  warn_for_twitter_status(r)
  if (r$status_code == 200L && parse) {
    r <- from_js(r)
  }
  r
}

warn_for_twitter_status <- function(x) {
  if (x$status_code != 200L) {
    w <- from_js(x)
    if (has_name_(w, "errors")) {
      warning(paste(w$errors, collapse = " - "), call. = FALSE,
        immediate. = TRUE)
    } else {
      warning(paste(w, collapse = " - "), call. = FALSE,
        immediate. = TRUE)
    }
    invisible(FALSE)
  } else {
    invisible(TRUE)
  }
}

#' Get the most recent direct messages sent to the authenticating user.
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
#' @param max_id optional Returns results with an ID less than (that
#'   is, older than) or equal to the specified ID.
#' @param n optional Specifies the number of direct messages to try
#'   and retrieve, up to a maximum of 200. The value of count is best
#'   thought of as a limit to the number of Tweets to return because
#'   suspended or deleted content is removed after the count has been
#'   applied.
#' @param parse Logical indicating whether to convert response object
#'   into nested list. Defaults to true.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find
#'   instructions on how to create tokens and setup an environment
#'   variable in the tokens vignette (in r, send \code{?tokens} to
#'   console).
#' @return Return object converted to nested list. If status code of
#'   response object is not 200, the response object is returned
#'   directly.
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
direct_messages <- function(since_id = NULL,
                            max_id = NULL,
                            n = 200,
                            parse = TRUE,
                            token = NULL) {
  query <- "direct_messages"
  token <- check_token(token, query)
  params <- list(include_entities = TRUE, count = n)
  url <- make_url(query = query, param = params)
  r <- httr::GET(url, token)
  warn_for_twitter_status(r)
  if (r$status_code == 200L && parse) {
    r <- from_js(r)
  }
  r
}

#' @inheritParams direct_messages
#' @export
#' @rdname direct_messages
direct_messages_sent <- function(since_id = NULL,
                                 max_id = NULL,
                                 n = 200,
                                 parse = TRUE,
                                 token = NULL) {
  query <- "direct_messages/sent"
  token <- check_token(token, query)
  params <- list(include_entities = TRUE, count = n)
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
      warning(paste(w$errors, collapse = " - "), call. = FALSE)
    } else {
      warning(w, call. = FALSE)
    }
  }
  x
}

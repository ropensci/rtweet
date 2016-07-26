#' TWIT
#'
#' @param get Locigical with the default, \code{get = TRUE},
#'   indicating whether the provided url should be passed along via
#'   a GET or POST request.
#' @param url Character vector designed to operate like
#'   \code{parse_url()} and \code{build_url()} functions in the
#'   httr package. The easiest way to do this is to work through
#'   the call-specific functions as they are designed to simplify
#'   the process. However, if one were interested in reverse-
#'   engingeering such a thing, I would recommend checking out
#'   \code{\link{makeurl}}.
#' @param Further named parameters, such as \code{token},
#'   \code{timeout}, etc, passed on to \code{modify_url} in
#'   httr. Unnamed parameters will be combined with
#'   {\link{config}}.
#' @note Occasionally Twitter does recommend using POST requests
#'   for data retrieval calls. This is usually the case when requests
#'   can involve long strings (containing up to 100 user_ids). For
#'   the most part, or at least for any function-specific requests
#'   (e.g., \code{get_friends}, take reflect these changes.
#'
#' @return Response (json) object
#' @import httr
#' @export
TWIT <- function(get = TRUE, url, ..., timeout = NULL,
                 filename = NULL, catch_error = FALSE) {

  if (get) {
    resp <- GET(url, ...)
  } else {
    if (!is.null(timeout)) {
      if (!is.null(filename)) {
        tryCatch(
          POST(url, ...,
            timeout(timeout),
            write_disk(filename, overwrite = TRUE)),
            error = function(e) return(invisible()))
        return(invisible())
      }
    } else {
      resp <- POST(url, ...)
    }
  }

  if (catch_error) stop_for_status(resp)

  resp
}

#' make_url
#'
#' @param restapi logical Default \code{restapi = TRUE}
#'   indicates the provided URL components should be
#'   specify Twitter's REST API. Set this to FALSE if you wish
#'   to make a request URL designed for Twitter's streaming api.
#' @param ver API version (default, 1.1, was most current
#'   at time of writing this package).
#' @param query Twitter's subsetting/topic identifiers.
#'   Although the httr package refers to this as "path",
#'   query is used here to maintain consistency with
#'   Twitter API's excellent documentation.
#' @param params Additional parameters (arguments) passed
#'   along. If none, NULL (default).
#' @return URL used in httr call.
#' @export
make_url <- function(restapi = TRUE, query, param = NULL) {
  if (restapi) {
    hostname <- "api.twitter.com"
  } else {
    hostname <- "stream.twitter.com"
  }

  alst <- structure(
    list(
      scheme = "https",
      hostname = hostname,
      port = NULL,
      path = paste0("1.1/", query, ".json"),
      query = param,
      params = NULL,
      fragment = NULL,
      username = NULL,
      password = NULL),
    class = "url")

  alst
}


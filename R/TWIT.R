#' TWIT
#'
#' @description Base function responsible for formulating GET and
#'   POST requests to Twitter API's.
#'
#' @param get Logical with the default, \code{get = TRUE},
#'   indicating whether the provided url should be passed along via
#'   a GET or POST request.
#' @param url Character vector designed to operate like
#'   parse_url and build_url functions in the httr package.
#'   The easiest way to do this is to work through
#'   the call-specific functions as they are designed to simplify
#'   the process. However, if one were interested in reverse-
#'   engingeering such a thing, I would recommend checking out
#'   \code{make_url}.
#' @param \dots Further named parameters, such as config, token,
#'   etc, passed on to modify_url in the httr package.
#' @note Occasionally Twitter does recommend using POST requests
#'   for data retrieval calls. This is usually the case when requests
#'   can involve long strings (containing up to 100 user_ids). For
#'   the most part, or at least for any function-specific requests
#'   (e.g., \code{get_friends}, take reflect these changes.
#' @return json response object
#' @importFrom httr GET POST timeout write_disk progress
#' @keywords internal
#' @noRd
TWIT <- function(get = TRUE, url, ...) {
  if (get) {
    return(GET(url, ...))
  } else {
    return(POST(url, ...))
  }
}

#' make_url
#'
#' @param restapi logical Default \code{restapi = TRUE}
#'   indicates the provided URL components should be
#'   specify Twitter's REST API. Set this to FALSE if you wish
#'   to make a request URL designed for Twitter's streaming api.
#' @param query Twitter's subsetting/topic identifiers.
#'   Although the httr package refers to this as "path",
#'   query is used here to maintain consistency with
#'   Twitter API's excellent documentation.
#' @param param Additional parameters (arguments) passed
#'   along. If none, NULL (default).
#' @return URL used in httr call.
#' @keywords internal
#' @noRd
make_url <- function(restapi = TRUE, query, param = NULL) {

  if (restapi) {
    hostname <- "api.twitter.com"
  } else {
    hostname <- "stream.twitter.com"
  }

  structure(
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
}

#' get_retweeters
#'
#' @description Returns ids of users who retweeted a specified
#'   Twitter status.
#'
#' @param status Numeric id of Twitter status (tweet) of interest.
#' @param page Character value specifying cursor value. By default
#'   this is set to \code{page = "-1"}, which means page 1. Value
#'   for the next page, if it exists, will be included in the
#'   return object.
#' @param token OAuth token (1.0 or 2.0). By default
#'   \code{token = NULL} fetches a non-exhausted token from
#'   an environment variable.
#' @param stringify Logical, indicating whether to return user
#'   ids as strings (some ids are too long to be read as numeric).
#'   Defaults to \code{TRUE}.
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#'
#' @return user ids
#' @noRd
get_retweeters <- function(status, page = "-1", token = NULL,
                           stringify = TRUE) {

  query = "statuses/retweeters/ids"

  params <- list(
    id = status,
    cursor = page,
    stringify = stringify)

  url <- make_url(restapi = TRUE, query, params)

  token <- check_token(token, query)

  resp <- TWIT(get = TRUE, url, token)

  ids <- from_js(resp)

  if (is.null(ids["ids"])) {
    return(list(ids = NA))
  }

  if (all(c("ids", "next_cursor_str") %in% names(ids))) {
    ids$next_cursor <- ids$next_cursor_str
    ids <- ids[c("ids", "next_cursor")]
  }

  ids
}

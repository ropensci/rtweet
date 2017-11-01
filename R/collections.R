#' Get collections by user or status id.
#'
#' Return data for specified collection (themed grouping of Twitter statuses).
#' Response data varies significantly compared to most other users and tweets
#' data objects returned in this package.
#'
#' @param id required The identifier of the Collection to return results for
#'   e.g., "custom-539487832448843776"
#' @param n Specifies the maximum number of results to include in
#'   the response. Specify count between 1 and 200.
#' @param parse Logical indicating whether to convert response object into
#'   nested list. Defaults to true.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find instructions on how
#'   to create tokens and setup an environment variable in the tokens vignette
#'   (in r, send \code{?tokens} to console).
#' @param ... Other arguments passed along to composed request query.
#' @return Return object converted to nested list if parsed otherwise
#'   an HTTP response object is returned.
#' @examples
#'
#' \dontrun{
#'
#' ## lookup a specific collection
#' cc <- lookup_collections("custom-539487832448843776")
#'
#' ## inspect data
#' str(cc)
#'
#' }
#'
#' @export
lookup_collections <- function(id, n = 200,
                               parse = TRUE,
                               token = NULL,
                               ...) {
  stopifnot(is.character(id), is_n(n))
  query <- "collections/entries"
  params <- list(
    id = id,
    count = n,
    ...
  )
  url <- make_url(query = query, param = params)
  token <- check_token(token)
  r <- httr::GET(url, token)
  warn_for_twitter_status(r)
  if (r$status_code == 200L && parse) {
    r <- from_js(r)
  }
  r
}


#' Get collections by user or status id.
#'
#' Find collections (themed grouping of statuses) created by specific user
#' or status id. Results include user, status, and collection features.
#'
#' @param user Screen name or user id of target user. Requests must
#'   provide a value for one of user or status_id.
#' @param status_id Optional, the identifier of the tweet for which to
#'   return results. Requests must provide a value for one of user or
#'   status_id.
#' @param n Maximum number of results to return. Defaults to 200.
#' @param cursor Page identifier of results to retrieve. If parse = TRUE,
#'   the next cursor value for any given request--if available--is stored
#'   as an attribute, accessible via \code{\link{next_cursor}}
#' @param parse Logical indicating whether to convert response object
#'   into nested list. Defaults to true.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find
#'   instructions on how to create tokens and setup an environment
#'   variable in the tokens vignette (in r, send \code{?tokens} to
#'   console).
#' @return Return object converted to nested list if parsed otherwise
#'   an HTTP response object is returned.
#' @examples
#'
#' \dontrun{
#'
#' ## lookup a specific collection
#' cnnc <- get_collections("cnn")
#'
#' ## inspect data
#' str(cnnc)
#'
#' ## by status id
#' wwe <- get_collections(status_id = "925172982313570306")
#'
#' ## inspect data
#' str(wwe)
#'
#' }
#'
#' @export
get_collections <- function(user,
                            status_id = NULL,
                            n = 200,
                            cursor = NULL,
                            parse = TRUE,
                            token = NULL) {
  query <- "collections/list"
  stopifnot(is_n(n))
  if (missing(user) && !is.null(status_id) ||
      is.null(user) && !is.null(status_id)) {
    stopifnot(is.atomic(status_id))
    params <- list(
      tweet_id = status_id,
      count = n,
      cursor = cursor
    )
  } else {
    stopifnot(is.atomic(user))
    params <- list(
      user = user,
      tweet_id = status_id,
      count = n,
      cursor = cursor
    )
    names(params)[1] <- .ids_type(params[[1]])
  }
  url <- make_url(query = query, param = params)
  token <- check_token(token)
  r <- httr::GET(url, token)
  warn_for_twitter_status(r)
  if (r$status_code == 200L && parse) {
    r <- from_js(r)
    attr(r, "next_cursor") <- r[["response"]][["cursors"]][["next_cursor"]]
  }
  r
}

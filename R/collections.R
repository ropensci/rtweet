#' Get collections by user or status id.
#'
#' Return data for specified collection (themed grouping of Twitter statuses).
#' Response data varies significantly compared to most other users and tweets
#' data objects returned in this package.
#'
#' @param id required. The identifier of the Collection to return results for
#'   e.g., "custom-539487832448843776"
#' @param n Specifies the maximum number of results to include in
#'   the response. Specify count between 1 and 200.
#' @inheritParams lookup_users
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
  
  params <- list(
    id = id,
    count = n,
    ...
  )
  TWIT_get(token, "/1.1/collections/entries", params, parse = parse)
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
#'   as an attribute, accessible via [next_cursor()]
#' @inheritParams lookup_users
#' @return Return object converted to nested list if parsed otherwise
#'   an HTTP response object is returned.
#' @examples
#' if (auth_has_default()) {
#'   cnnc <- get_collections("cnn")
#'   str(cnnc$objects$users[[1]])
#' }
#' @export
get_collections <- function(user,
                            status_id = NULL,
                            n = 200,
                            cursor = NULL,
                            parse = TRUE,
                            token = NULL) {
  stopifnot(is_n(n))
  
  params <- list(
    count = n,
    cursor = cursor
  )
  
  if (missing(user) && !is.null(status_id) ||
      is.null(user) && !is.null(status_id)) {
    stopifnot(is.atomic(status_id))
    
    params$tweet_id <- status_id
  } else {
    stopifnot(is.atomic(user))
    
    params[[user_type(user)]] <- user
  }
  
  r <- TWIT_get(token, "/1.1/collections/list", params, parse = parse)
  if (parse) {
    attr(r, "next_cursor") <- r[["response"]][["cursors"]][["next_cursor"]]
  }
  r
}

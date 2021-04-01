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
#' @inheritParams TWIT_paginate_cursor 
#' @param user Screen name or user id of target user. Requests must
#'   provide a value for one of user or status_id.
#' @param status_id Optional, the identifier of the tweet for which to
#'   return results. Requests must provide a value for one of user or
#'   status_id.
#' @inheritParams lookup_users
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
#' @references <https://developer.twitter.com/en/docs/twitter-api/v1/tweets/curate-a-collection/api-reference/get-collections-list>
#' @export
get_collections <- function(user = NULL,
                            status_id = NULL,
                            n = 200,
                            cursor = NULL,
                            parse = TRUE,
                            token = NULL) {
  
  params <- list()
  if (is.null(user) && !is.null(status_id)) {
    stopifnot(is.atomic(status_id))
    
    params$tweet_id <- status_id
  } else {
    stopifnot(is.atomic(user))
    
    params[[user_type(user)]] <- user
  }
  
  r <- TWIT_paginate_cursor(token, "/1.1/collections/list", params, 
    n = n,
    page_size = 200,
    cursor = cursor,
    get_id = function(x) x$response$results$timeline_id
  )
    
  r
}

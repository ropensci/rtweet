#' Get the most recent retweets of a specific Twitter status
#'
#' Returns a collection of the 100 most recent retweets of a given
#' status.  NOTE: Twitter's API is currently limited to 100 or fewer
#' retweeters.
#'
#' @inheritParams lookup_users
#' @param status_id required The numerical ID of the desired status.
#' @param n optional Specifies the number of records to retrieve.
#'   Must be less than or equal to 100.
#' @param ... Other arguments used as parameters in the query sent to
#'   Twitter's rest API, for example, `trim_user = TRUE`.
#' @return Tweets data of the most recent retweets of a given status
#' @export
#' @family retweets
get_retweets <- function(status_id, n = 100, parse = TRUE, token = NULL, ...) {
  stopifnot(is.character(status_id), length(status_id) == 1L)
  
  query <- sprintf("/1.1/statuses/retweets/%s", status_id)
  params <- list(
    id = status_id,
    count = n,
    ...
  )
  r <- TWIT_get(token, query, params, parse = parse)
  
  if (parse) {
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
#' @inheritParams lookup_users
#' @param status_id required The status ID of the desired status.
#' @param n Specifies the number of records to retrieve.  Best if
#'   intervals of 100.
#' @return data
#' @family retweets
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/v1/tweets/post-and-engage/api-reference/get-statuses-retweeters-ids>
get_retweeters <- function(status_id,
                           n = 100,
                           parse = TRUE,
                           token = NULL) {
  
  
  params <- list(
    id = status_id,
    stringify_ids = TRUE
  )

  r <- TWIT_paginate_cursor(token, "/1.1/statuses/retweeters/ids", 
    params = params,
    page_size = 100
  )
  
  if (parse) {
    r <- parse_retweeters(r)
  }
  r
}


parse_retweeters <- function(x) {
  ids <- lapply(x, function(x) x$ids)
  tibble::tibble(user_id = unlist(ids))
}

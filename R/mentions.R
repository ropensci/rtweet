#' Get mentions for the authenticating user.
#'
#' Returns data on up to 200 of the most recent mentions (Tweets
#' containing a users's screen_name) of the authenticating user.
#' The timeline returned is the equivalent of the one seen when you view 
#' your mentions on twitter.com.
#'
#' @inheritParams TWIT_paginate_max_id
#' @param ... Other arguments passed as parameters in composed API
#'   query.
#' @return Tibble of mentions data.
#' @family tweets
#' @examples
#'
#' \dontrun{
#'
#' ## get most recent 200 mentions of authenticating user
#' mymentions <- get_mentions()
#'
#' ## view data
#' mymentions
#'
#' }
#'
#' @seealso
#'   <https://developer.twitter.com/en/docs/tweets/timelines/api-reference/get-statuses-mentions_timeline>
#' @export
get_mentions <- function(n = 200,
                         since_id = NULL,
                         max_id = NULL,
                         parse = TRUE,
                         token = NULL,
                         ...) {
  
  message("Getting mentions for ", api_screen_name(token))
  
  params <- list(
    count = n,
    since_id = since_id,
    max_id = max_id,
    ...
  )
  r <- TWIT_get(token, "/1.1/statuses/mentions_timeline", params)
  
  if (parse) {
    r <- parse_mentions(r)
    if (has_name_(r, "created_at")) {
      r$created_at <- format_date(r$created_at)
    }
  }
  r
}

parse_mentions <- function(x) {
  out <- tibble::as_tibble(wrangle_into_clean_data(x, "status"))
  if (has_name_(x, "user")) {
    users <- tibble::as_tibble(wrangle_into_clean_data(x, "user"))
    attr(out, "users") <- users
  }
  out
}

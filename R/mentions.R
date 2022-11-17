#' Get mentions for the authenticating user.
#'
#' Returns data on up to 200 of the most recent mentions (Tweets
#' containing a users' screen_name) of the authenticating user.
#' The timeline returned is the equivalent of the one seen when you view
#' your mentions on twitter.com.
#'
#' @inheritParams TWIT_paginate_max_id
#' @param ... Other arguments passed as parameters in composed API
#'   query.
#' @return Tibble of mentions data.
#' @family tweets
#' @examples
#' if (auth_has_default()) {
#'   tw <- get_mentions()
#'   tw
#'
#'   # newer mentions
#'   get_mentions(since_id = tw)
#' }
#' @references <https://developer.twitter.com/en/docs/twitter-api/v1/tweets/timelines/overview>
#' @export
get_mentions <- function(n = 200,
                         since_id = NULL,
                         max_id = NULL,
                         parse = TRUE,
                         retryonratelimit = NULL,
                         verbose = TRUE,
                         token = NULL,
                         ...) {

  params <- list(...)
  r <- TWIT_paginate_max_id(token, "/1.1/statuses/mentions_timeline", params,
    n = n,
    since_id = since_id,
    max_id = max_id,
    retryonratelimit = retryonratelimit,
    verbose = verbose
  )

  if (parse) {
    r <- tweets_with_users(r)
    r$created_at <- format_date(r$created_at)
  }
  r
}

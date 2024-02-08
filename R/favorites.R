#' Get tweets liked/favorited by one or more users
#'
#' Returns up to 3,000 tweets liked/favorited for each `user`.
#' `r lifecycle::badge("deprecated")`
#'
#' @inheritParams TWIT_paginate_max_id
#' @inheritParams get_timeline
#' @inheritParams stream
#' @return A tibble with one row for each tweet.
#' @examples
#' if (FALSE) {
#'   get_favorites("KFC")
#' }
#' @family tweets
#' @seealso [`rtweet-deprecated`]
#' @references <https://developer.twitter.com/en/docs/twitter-api/v1/tweets/post-and-engage/api-reference/get-favorites-list>
#' @export
get_favorites <- function(user,
                          n = 200,
                          since_id = NULL,
                          max_id = NULL,
                          parse = TRUE,
                          retryonratelimit = NULL,
                          verbose = TRUE,
                          token = NULL) {
  rt <- lapply(user, get_favorites_user,
    n = n,
    since_id = since_id,
    max_id = max_id,
    parse = parse,
    retryonratelimit = retryonratelimit,
    verbose = verbose,
    token = token
  )

  if (parse) {
    rt <- do_call_rbind(rt)
  }
  rt
}

get_favorites_user <- function(user, ..., parse = TRUE, token = NULL) {
  stopifnot(length(user) == 1)
  params <- list(
    # Undocumented parameter https://github.com/ropensci/rtweet/issues/575#issuecomment-829605892
    tweet_mode = "extended"
  )
  params[[user_type(user)]] <- user

  results <- TWIT_paginate_max_id(token, "/1.1/favorites/list", params,
    page_size = 200,
    ...
  )

  if (parse) {
    results <- tweets_with_users(results)
    results$created_at <- format_date(results$created_at)
    results$favorited_by <- rep(user, nrow(results))
  }

  results
}

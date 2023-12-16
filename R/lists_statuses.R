#' Get a timeline of tweets authored by members of a specified list.
#'
#' @param list_id required The numerical id of the list.
#' @param slug required You can identify a list by its slug instead of
#'   its numerical id. If you decide to do so, note that you'll also have
#'   to specify the list owner using the owner_id or owner_screen_name
#'   parameters.
#' @param owner_user optional The screen name or user ID of the user
#'   who owns the list being requested by a slug.
#' @param include_rts optional When set to either true, t or 1,
#'   the list timeline will contain native retweets (if they exist) in
#'   addition to the standard stream of tweets. The output format of
#'   retweeted tweets is identical to the representation you see in
#'   home_timeline.
#' @inheritParams TWIT_paginate_max_id
#' @inheritParams stream
#' @family lists
#' @family tweets
#' @return data
#' @export
lists_statuses <- function(list_id = NULL,
                           slug = NULL,
                           owner_user = NULL,
                           since_id = NULL,
                           max_id = NULL,
                           n = 200,
                           include_rts = TRUE,
                           parse = TRUE,
                           retryonratelimit = NULL,
                           verbose = TRUE,
                           token = NULL) {
  params <- lists_params(
    list_id = list_id,
    slug = slug,
    owner_user = owner_user,
    count = n,
    include_rts = include_rts,
    # Undocumented parameter https://github.com/ropensci/rtweet/issues/575#issuecomment-829605892
    tweet_mode = "extended"
  )

  results <- TWIT_paginate_max_id(token, "/1.1/lists/statuses", params,
    page_size = 200,
    max_id = max_id,
    since_id = since_id,
    n = n,
    retryonratelimit = retryonratelimit,
    verbose = verbose
  )

  if (parse) {
    results <- tweets_with_users(results)
    results$created_at <- format_date(results$created_at)
  }
  results
}

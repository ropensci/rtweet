#' Get a timeline of tweets authored by members of a specified list.
#'
#' @param list_id required The numerical id of the list.
#' @param slug required You can identify a list by its slug instead of
#'   its numerical id. If you decide to do so, note that you'll also have
#'   to specify the list owner using the owner_id or owner_screen_name
#'   parameters.
#' @param owner_user optional The screen name or user ID of the user
#'   who owns the list being requested by a slug.
#' @param since_id optional Returns results with an ID greater than
#'   (that is, more recent than) the specified ID. There are limits to the
#'   number of Tweets which can be accessed through the API. If the limit
#'   of Tweets has occurred since the since_id, the since_id will be forced
#'   to the oldest ID available.
#' @param max_id optional Returns results with an ID less than (that is,
#'   older than) or equal to the specified ID.
#' @param n optional Specifies the number of results to retrieve per "page."
#' @param include_rts optional When set to either true, t or 1,
#'   the list timeline will contain native retweets (if they exist) in
#'   addition to the standard stream of tweets. The output format of
#'   retweeted tweets is identical to the representation you see in
#'   home_timeline.
#' @inheritParams lookup_users
#' @family lists
#' @family tweets
#' @return data
#' @examples
#' if (default_has_auth()) {
#'   senator_tweets <- lists_statuses(slug = "senators", owner_user = "cspan")
#'   senator_tweets
#' } 
#' @export
lists_statuses <- function(list_id = NULL,
                           slug = NULL,
                           owner_user = NULL,
                           since_id = NULL,
                           max_id = NULL,
                           n = 200,
                           include_rts = TRUE,
                           parse = TRUE,
                           token = NULL) {
  params <- lists_params(
    list_id = list_id,
    slug = slug,
    owner_user = owner_user,
    since_id = since_id,
    max_id = max_id,
    count = n,
    include_rts = include_rts,
    tweet_mode = "extended"
  )

  results <- TWIT_paginate_max_id(token, "/1.1/lists/statuses", params,
    get_max_id = function(x) x$id_str,
    page_size = 200,
    n = n,
    parse = parse
  )
  
  if (parse) {
    results <- tweets_with_users(results)
  }
  results
}

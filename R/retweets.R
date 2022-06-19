#' Get the most recent retweets/retweeters
#'
#' `get_retweets()` returns the 100 most recent retweets of a tweet;
#' `get_retweeters()` retursn the 100 most recent users who retweeted them.
#'
#' @inheritParams lookup_users
#' @param status_id Tweet/status id.
#' @param n Number of results to retrieve. Must be <= 100.
#' @param ... Other arguments used as parameters in the query sent to
#'   Twitter's rest API, for example, `trim_user = TRUE`.
#' @return Tweets data of the most recent retweets/retweeters of a given status
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/v1/tweets/post-and-engage/api-reference/get-statuses-retweets-id>
get_retweets <- function(status_id, n = 100, parse = TRUE, token = NULL, ...) {
  stopifnot(is.character(status_id), length(status_id) == 1L)
  
  query <- sprintf("/1.1/statuses/retweets/%s", status_id)
  params <- list(
    id = status_id,
    count = n,
    # Undocumented parameter https://github.com/ropensci/rtweet/issues/575#issuecomment-829605892
    tweet_mode = "extended",
    ...
  )
  r <- TWIT_get(token, query, params)
  
  if (parse) {
    r <- tweets_with_users(list(r))
    r$created_at <- format_date(r$created_at)
  }
  r
}

#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/v1/tweets/post-and-engage/api-reference/get-statuses-retweeters-ids>
#' @rdname get_retweets
get_retweeters <- function(status_id, n = 100, parse = TRUE, token = NULL) {
  params <- list(
    id = status_id,
    count = n,
    stringify_ids = TRUE
  )
  r <- TWIT_get(token, "/1.1/statuses/retweeters/ids", params)
  
  if (parse) {
    r <- tibble::tibble(user_id = r$ids)
  }
  r
}

#' Get users data on accounts identified via search query.
#'
#' Returns data for up to 1,000 users matched by user provided search
#' query.
#'
#' @inheritParams lookup_users
#' @param q Query to be searched, used in filtering relevant tweets to
#'   return from Twitter's REST API. Should be a character string not
#'   to exceed 500 characters maximum. Spaces are assumed to function
#'   like boolean "AND" operators. To search for tweets including one
#'   of multiple possible terms, separate search terms with spaces and
#'   the word "OR". For example, the search `query =
#'   "data science"` searches for tweets using both "data" and
#'   "science" though the words can appear anywhere and in any order
#'   in the tweet. However, when OR is added between search terms,
#'   `query = "data OR science"`, Twitter's REST API should
#'   return any tweet that includes either "data" or "science"
#'   appearing in the tweets. At this time, Twitter's users/search API
#'   does not allow complex searches or queries targeting exact
#'   phrases as is allowed by `search_tweets`.
#' @param n Number of users to return (at most 1,000).
#' @param verbose If `TRUE`, will display a progress bar while downloading.
#' @examples
#'
#' \dontrun{
#' users <- search_users("#rstats", n = 300)
#' users
#'
#' # latest tweet from each user
#' tweets_data(users)
#' }
#'
#' @return Data frame of users returned by query.
#' @family users
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/v1/accounts-and-users/follow-search-get-users/api-reference/get-users-search>
search_users <- function(q, n = 100,
                         parse = TRUE,
                         token = NULL,
                         verbose = TRUE) {
  
  stopifnot(is_n(n), is.atomic(q))
  if (n > 1000) {
    abort("`n` must be <= 1,000")
  }

  pages <- ceiling(n / 20)
  results <- vector("list", pages)

  if (verbose) {
    pb <- progress::progress_bar$new(
      format = "Searching for users :bar",
      total = pages
    ) 
    withr::defer(pb$terminate())
  }
  
  params <- list(q = q, count = 20)
  for (i in seq_len(pages)) {
    if (verbose) {
      pb$tick()
    }  
    params$page <- i
    results[[i]] <- TWIT_get(token, "/1.1/users/search", params)
  }
  
  if (parse) {
    results <- users_with_tweets(results)
  }
  
  results
}

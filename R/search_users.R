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
#' @param n Numeric, specifying the total number of desired users to
#'   return. Defaults to 100. Maximum number of users returned from a
#'   single search is 1,000.
#' @param verbose If `TRUE`, will display a progress bar while downloading.
#' @references <https://dev.twitter.com/overview/documentation>
#' @examples
#'
#' \dontrun{
#'
#' ## search for up to 1000 users using the keyword rstats
#' rstats <- search_users(q = "rstats", n = 1000)
#'
#' ## data frame where each observation (row) is a different user
#' rstats
#'
#' ## tweets data also retrieved. can access it via tweets_data()
#' tweets_data(rstats)
#'
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
    warning(
      "search only returns up to 1,000 users per ",
      "unique search. Setting n to 1000..."
    )
    n <- 1000
  }

  if (nchar(q) > 500) {
    stop("q cannot exceed 500 characters.", call. = FALSE)
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
    results <- tweets_with_users(results)
  }
  
  results
}

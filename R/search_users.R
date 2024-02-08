#' Search for users
#'
#' Search for Twitter users. The Twitter API limits the results to at most
#' 1,000 users.  `r lifecycle::badge("deprecated")`
#'
#' @inheritParams TWIT_paginate_max_id
#' @inheritParams stream
#' @param q As string providing the search query. Try searching by interest,
#'   full name, company name, or location. Exact match searches are not
#'   supported.
#' @return Data frame with one row for each matching user.
#' @family users
#' @seealso [user_search()], [`rtweet-deprecated`]
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/v1/accounts-and-users/follow-search-get-users/api-reference/get-users-search>
search_users <- function(q, n = 100,
                         parse = TRUE,
                         token = NULL,
                         verbose = TRUE) {

  stopifnot(is_n(n), is.atomic(q) && !is.null(q))
  if (n > 1000) {
    abort("`n` must be <= 1,000 (the maximum allowed by Twitter)")
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
    results$created_at <- format_date(results$created_at)
  }

  results
}

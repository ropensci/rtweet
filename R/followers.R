#' Get user IDs for accounts following target user.
#'
#' Returns a list of user IDs for the accounts following specified
#' user.
#'
#' @inheritParams get_timeline
#' @inheritParams TWIT_paginate_cursor
#' @param n Number of followers to return. Use `Inf` to download all followers.
#' 
#'   Results are downloaded in pages of 5000, and you can download 15 pages
#'   (i.e. 75,000 tweets) in each 15 minute period. The easiest way to download 
#'   more than that is to set `retryonratelimit = TRUE`.
#' @param page `r lifecycle::badge("deprecated")` Please use `cursor` instead.
#' @seealso
#'   <https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-followers-ids>
#' @examples
#' if (auth_has_default()) {
#'   user <- get_followers("KFC", n = 100)
#'   user
#' }
#' @return A tibble data frame of follower IDs (one column named "user_id").
#' @family ids
#' @export
get_followers <- function(user, n = 5000,
                          cursor = "-1",
                          retryonratelimit = FALSE,
                          parse = TRUE,
                          verbose = TRUE,
                          token = NULL,
                          page = lifecycle::deprecated()) {
  
  if (lifecycle::is_present(page)) {
    lifecycle::deprecate_warn("1.0.0", "get_followers(page)", "get_followers(cursor)")
    cursor <- page
  }

  stopifnot(is.atomic(user), isTRUE(length(user) == 1))
  
  params <- list(stringify_ids = TRUE)
  params[[user_type(user)]] <- user

  results <- TWIT_paginate_cursor(token, "/1.1/followers/ids", params, 
    page_size = 5000, 
    n = n,
    retryonratelimit = retryonratelimit,
    cursor = cursor,
    verbose = verbose
  )
  
  if (parse) {
    results <- lapply(results, parse.piper.fs, n = n)
    results <- do.call("rbind", results)
  }

  results
}

parse.piper.fs <- function(f, n = NULL) {
  if (!is.list(f)) {
    f <- list(f)
  }
  if (length(f) == 0L) {
    return(data.frame())
  }
  df <- unlist(lapply(f, "[[[", "ids"), use.names = FALSE)
  if (length(df) == 0L) {
    return(data.frame())
  }

  df <- as_tbl(list(user_id = df))
  if (!is.null(n)) {
    if (n < nrow(df)) {
      df <- df[seq_len(n), ]
    }
  }
  df
}

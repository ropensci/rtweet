#' Get user IDs for accounts following target user.
#'
#' Returns a list of user IDs for the accounts following specified
#' user.
#'
#' @inheritParams TWIT_paginate_cursor
#' @inheritParams get_timeline
#' @param page `r lifecycle::badge("deprecated")` Please use `cursor` instead.
#' @references <https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-followers-ids>
#' @examples
#'
#' \dontrun{
#' users <- get_followers("KFC")
#' users
#' 
#' # use `cursor` to find the next "page" of results
#' more_users <- get_followers("KFC", cursor = users)
#'
#' }
#' @return A tibble data frame with one column named "follower_of" with the 
#' followers and another one "user_id" with the user used as input.
#' @export
get_followers <- function(user, n = 5000,
                          cursor = "-1",
                          retryonratelimit = NULL,
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
    cursor = cursor,
    retryonratelimit = retryonratelimit,
    verbose = verbose
  )
  
  if (parse) {
    df <- tibble::tibble(follower_of = unlist(lapply(results, function(x) x$ids)),
                         user_id = user)
    results <- copy_cursor(df, results)
  }
  results
}

#' Get user IDs for accounts following target user.
#'
#' Returns a list of user IDs for the accounts following specified
#' user. `r lifecycle::badge("deprecated")`
#'
#' @inheritParams TWIT_paginate_cursor
#' @inheritParams get_timeline
#' @inheritParams stream
#' @param page `r lifecycle::badge("deprecated")` Please use `cursor` instead.
#' @seealso [`rtweet-deprecated`]
#' @references <https://developer.twitter.com/en/docs/twitter-api/v1/accounts-and-users/follow-search-get-users/api-reference/get-followers-ids>
#' @examples
#' if (FALSE) {
#'   users <- get_followers("_R_Foundation")
#' }
#' @return A tibble data frame with one column named "from_id" with the
#' followers and another one "to_id" with the user used as input.
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

  stopifnot(is.atomic(user) && !is.null(user), isTRUE(length(user) == 1))

  params <- list(stringify_ids = TRUE)
  params[[user_type(user)]] <- user

  results <- TWIT_paginate_cursor(token, "/1.1/followers/ids", params,
    page_size = if (n >= 5000) 5000 else n,
    n = n,
    cursor = cursor,
    retryonratelimit = retryonratelimit,
    verbose = verbose
  )

  if (parse) {
    df <- tibble::tibble(from_id = unlist(lapply(results, function(x) x$ids)),
                         to_id = user)
    results <- copy_cursor(df, results)
    class(results) <- c("followers", class(results))
  }
  results
}

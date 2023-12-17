#' Get list subscriptions of a given user but does not include the user's own
#' lists.
#'
#' @inheritParams TWIT_paginate_cursor
#' @inheritParams get_timeline
#' @family lists
#' @references <https://developer.twitter.com/en/docs/twitter-api/v1/accounts-and-users/create-manage-lists/api-reference/get-lists-subscriptions>
#' @export
lists_subscriptions <- function(user,
                                n = 20,
                                cursor = "-1",
                                parse = TRUE,
                                retryonratelimit = NULL,
                                verbose = TRUE,
                                token = NULL) {

  params <- list(
    count = n,
    cursor = cursor
  )
  params[[user_type(user)]] <- user

  r <- TWIT_paginate_cursor(token, "/1.1/lists/subscriptions", params,
    cursor = cursor,
    retryonratelimit = retryonratelimit,
    verbose = verbose,
    page_size = if (n >= 1000) 1000 else n,
    get_id = function(x) x$user_id
  )

  if (parse) {
    out <- parse_lists_list(r)
  }
  out
}

#' Get list subscriptions of a given user.
#'
#' @inheritParams TWIT_paginate_cursor
#' @inheritParams get_timeline
#' @examples
#'
#' \dontrun{
#'
#' ## get kearneymw subscriptions
#' rstats <- lists_subscriptions(
#'   user = "kearneymw",
#'   n = 1000
#' )
#'
#' }
#'
#' @family lists
#' @export
lists_subscriptions <- function(user,
                                n = 20,
                                cursor = "-1",
                                parse = TRUE,
                                token = NULL) {
  
  params <- list(
    count = n,
    cursor = cursor
  )
  params[[user_type(user)]] <- user

  r <- TWIT_paginate_cursor(token, "/1.1/lists/subscriptions", params,
    page_size = 1000,
    cursor = cursor,
    get_id = function(x) x$user_id
  )
  
  if (parse) {
    out <- parse_lists_list(r)
  }
  out
}

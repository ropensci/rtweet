#' Get Twitter users data for given users (user IDs or screen names).
#'
#' @inheritParams TWIT_paginate_max_id
#' @inheritParams stream
#' @param users User id or screen name of target user.
#' @references <https://developer.twitter.com/en/docs/twitter-api/v1/accounts-and-users/follow-search-get-users/api-reference/get-users-lookup>
#'
#' @examples
#'
#' if (auth_has_default()) {
#'     users <- c("twitter", "rladiesglobal", "_R_Foundation")
#'     users <- lookup_users(users)
#'     users
#'
#'     # latest tweet from each user
#'     tweets_data(users)
#' }
#'
#' @return A tibble of users data.
#' @family users
#' @export
lookup_users <- function(users, parse = TRUE, token = NULL,
                         retryonratelimit = NULL,
                         verbose = TRUE) {
  type <- user_type(users, "users")

  chunks <-  unname(split(users, (seq_along(users) - 1) %/% 100))
  params_list <- lapply(chunks, function(users) {
    params <- list()
    params[[type]] <- paste0(users, collapse = ",")
    params
  })

  results <- TWIT_paginate_chunked(token, "/1.1/users/lookup", params_list,
    retryonratelimit = retryonratelimit,
    verbose = verbose
  )

  if (parse) {
    results <- users_with_tweets(results)
    results$created_at <- format_date(results$created_at)
  }

  results
}

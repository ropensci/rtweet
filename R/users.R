#' Get Twitter users data for given users (user IDs or screen names).
#'
#' @inheritParams TWIT_paginate_max_id
#'   
#' @param users User id or screen name of target user.
#' @references <https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-users-lookup>
#' 
#' @examples
#'
#' \dontrun{
#'
#' ## select one or more twitter users to lookup
#' users <- c(
#'   "potus", "hillaryclinton", "realdonaldtrump",
#'   "fivethirtyeight", "cnn", "espn", "twitter"
#' )
#' usr_df <- lookup_users(users)
#'
#' ## view tweet data for these users via tweets_data()
#' tweets_data(usr_df)
#' 
#' # Find user data for the recent posters to #rstats
#' rs <- search_tweets("#rstats", n = 500)
#' user_ids <- unique(rs$user_id)
#' lookup_users(user_ids)
#' }
#'
#' @return A tibble of users data.
#' @family users
#' @export
lookup_users <- function(users, parse = TRUE, token = NULL,
                         retryonratelimit = FALSE,
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
  }
  results
}

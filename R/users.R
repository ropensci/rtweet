#' Get Twitter users data for given users (user IDs or screen names).
#'
#' @inheritParams TWIT_paginate_max_id
#' @param token Expert use only. Use this to override authentication for
#'   a single API call. In most cases you are better off changing the
#'   default for all calls. See [auth_as()] for details.
#' @param parse The default, `TRUE`, indicates that the result should
#'   be parsed into a convenient R data structure like a list or data frame. 
#'   This protects you from the vagaries of the twitter API. Use `FALSE` 
#'   to return the "raw" list produced by the JSON returned from the twitter 
#'   API.
#'   
#' @param users User id or screen name of target user.
#' @seealso <https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-users-lookup>
#' 
#' @examples
#' if (auth_has_default()) {
#'   # Find user data for the recent posters to #rstats
#'   rs <- search_tweets("#rstats", n = 500)
#'   user_ids <- unique(rs$user_id)
#'   lookup_users(user_ids)
#' }
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

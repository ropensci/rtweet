#' Get Twitter users data for given users (user IDs or screen names).
#'
#' Returns data on up to 90,000 Twitter users. To return data on more
#' than 90,000 users, code must be written to iterate through user IDs
#' whilst avoiding rate limits, which reset every 15 minutes.
#'
#' @param token An OAuth token object. The default, `NULL`, will retrieve
#'   a default token with [get_token()]. You should only need to 
#'   use this argument if you are wrapping rtweet in a package. 
#'   
#'   See `vignette("auth")` for more details.
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
lookup_users <- function(users, parse = TRUE, token = NULL) {
  stopifnot(is.atomic(users))
  
  if (length(users) > 90000L) {
    message("max number of users exceeded; looking up first 90,000")
    users <- users[1:90000]
  }
  
  chunks <-  unname(split(users, (seq_along(users) - 1) %/% 100))
  results <- lapply(chunks, user_lookup_100, token = token)
  
  if (parse) {
    results <- users_with_tweets(results)
  }
  results
}

user_lookup_100 <- function(users, token = NULL) {
  stopifnot(length(users) <= 100)
  
  params <- list()
  params[[.id_type(users)]] <- paste0(users, collapse = ",")
  TWIT_get(token, "/1.1/users/lookup", params)
}

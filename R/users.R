
.user_lookup <- function(users, token = NULL) {

  if (class(users) == "list") {
    users <- unlist(users)
  }

  if (length(users) > 100) {
    users <- users[1:100]
  }

  params <- list(user_id = paste(users, collapse = ","))

  url <- make_url(
    restapi = TRUE,
    "users/lookup",
    params)

  token <- check_token(token, query = "users/lookup")

  resp <- TWIT(get = TRUE, url, token)

  resp <- from_js(resp)

  user_df(resp)
}


#' lookup_users
#'
#' @description Returns Twitter user data_frame object for
#'   specified user_ids or screen_names.
#'
#' @param users User id or screen name of target user.
#' @param token OAuth token (1.0 or 2.0). By default
#'   \code{token = NULL} fetches a non-exhausted token from
#'   an environment variable @describeIn tokens.
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#' @examples
#' \dontrun{
#' # search for 1000 tweets mentioning Hillary Clinton
#' hrc <- search_tweets(q = "hillaryclinton", count = 1000)
#'
#' # lookup returned user_id values
#' users <- lookup_users(hrc$user_id)
#' users
#'
#' # merge data objects
#' dat <- dplyr::left_join(hrc, users, by = "user_id")
#' dat
#' }
#'
#' @return json response object (max is 18000 per token)
#' @importFrom dplyr bind_rows data_frame
#' @export
lookup_users <- function(users, token = NULL) {

  if (length(users) > 18000) {
    users <- users[1:18000]
  }

  increments <- 1:ceiling(length(users) / 100)

  from <- 1

  usr_df <- data_frame()

  for (i in increments) {
    to <- from + 99

    usr_new <- .user_lookup(
      users[from:to],
      token)

    usr_df <- bind_rows(
      usr_df,
      usr_new)

    from <- to + 1
  }

  usr_df
}

#' user_lookup
#'
#' @param users Screen name or user id of target users.
#' @param token OAuth tokens (1.0 or 2.0)
#' @param df logical, indicating whether to format response
#' as data frame
#' @seealso See \url{https://dev.twitter.com/overview/
#' documentation} for more
#'   information on using Twitter's API.
#' @return response object
#' @export
user_lookup <- function(user_ids, token = NULL) {

  if (class(user_ids) == "list") {
    user_ids <- unlist(user_ids)
  }

  if (length(user_ids) > 100) {
    user_ids <- user_ids[1:100]
  }

  params <- list(id = paste(user_ids, collapse = ","))

  url <- make_url(
    restapi = TRUE,
    "users/lookup",
    params)

  if (is.null(token)) {
    token <- get_tokens()
    token <- fetch_tokens(token, "friends/ids")
  }

  resp <- TWIT(get = TRUE, url, token)

  resp <- from_js(resp)

  parse_user(resp)
}


#' lookup_users
#'
#' @param ids User id or screen name of target user.
#' @param token OAuth tokens (1.0 or 2.0)
#' @seealso See \url{https://dev.twitter.com/overview/
#'   documentation} for more information on using
#'   Twitter's API.
#' @return response object (max is 18000 per token)
#' @import dplyr
#' @export
lookup_users <- function(ids, token = NULL) {

  if (class(ids) == "list") {
    user_ids <- unlist(ids)
  }

  if (length(ids) > 18000) {
    ids <- ids[1:18000]
  }

  reqs <- 1:ceiling(length(ids) / 100)
  first <- 1

  usr_df <- dplyr::data_frame()

  for (i in reqs) {
    last <- first + 99

    usr_new <- user_lookup(
      ids[first:last],
      token)

    usr_df <- dplyr::bind_rows(
      usr_df,
      usr_new)

    first <- last + 1
  }

  usr_df
}

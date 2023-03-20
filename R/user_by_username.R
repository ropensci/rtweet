#' Search users by username
#'
#' Looks up users by their username.
#' @inheritParams filtered_stream
#' @param ... Other arguments passed to the API.
#' @param verbose A logical value to provide more information about paginated queries.
#' @param username A user name string or up to 100.
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/users/lookup/api-reference/get-users-by-username-username>
#' <https://developer.twitter.com/en/docs/twitter-api/users/lookup/api-reference/get-users-by>
#' @seealso [user_search()]
#' @examples
#' if (FALSE) {
#'   user_by_username("rOpenSci")
#'   user_by_username(c("Bioconductor", "R_Contributors"))
#' }
user_by_username <- function(username, expansions = NULL, fields = NULL, ...,
                        token = NULL, parse = TRUE, verbose = FALSE) {

  expansions <- check_expansions(arg_def(expansions, "pinned_tweet_id"),
                                 "pinned_tweet_id")
  fields <- check_fields(arg_def(fields, set_fields(media = NULL,
                                                    poll = NULL,
                                                    place = NULL)),
                         metrics = NULL, place = NULL, poll = NULL, media = NULL)
  expansions_for_fields(expansions, fields)
  if (!is_logical(verbose)) {
    abort("`verbose` must be either `TRUE` or `FALSE`.")
  }
  parsing(parse, expansions, fields)
  if (length(username) > 100 || !is_user_id(username) && length(username) == 0) {
    abort("Please introduce less than 100 valid user ids.")
  }
  if (length(username) > 1) {
    data <- c(list(usernames = username, expansions = expansions), fields, ...)
    url <- "users/by"
  } else {
    data <- c(list(expansions = expansions), fields, ...)
    url <- paste0("users/by/username/", username)
  }
  data <- unlist(prepare_params(data), recursive = FALSE)
  data <- data[data != ""]

  # Rates from the website app and user limits
  token <- check_token_v2(token, c("bearer", "pkce"))
  check_scopes_token(token, c("tweet.read", "users.read"))
  rate <- check_rate(token, 300/(60*15), 900/(60*15))
  req_archive <- endpoint_v2(token, url, rate)
  req_final <- httr2::req_url_query(req_archive, !!!data)
  p <- pagination(req_final, 1, 1, verbose)
  if (!parse) {
    return(p)
  }
  parse(p, expansions, fields)
}

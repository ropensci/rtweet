#' Tweets from a user
#'
#' Looks up tweets posted by a user.
#' @inheritParams filtered_stream
#' @param ... Other arguments passed to the API.
#' @param verbose A logical value to provide more information about paginated queries.
#' @returns
#' A data.frame with the id, name and username of the authenticated user.
#' Other information depends on the `expansions` and `fields` requested.
#' Accepted values are:
#' - Expansions: `set_expansions(tweet = NULL, list = NULL)`.
#' - Fields: `set_fields(media = NULL, poll = NULL, place = NULL)`
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/users/lookup/api-reference/get-users-me>
#' @examples
#' if (FALSE) {
#'   me <- user_self()
#' }
user_self <- function(expansions = NULL, fields = NULL, ...,
                        token = NULL, parse = TRUE, verbose = FALSE) {

  expansions <- check_expansions(arg_def(expansions, "pinned_tweet_id"),
                                 "pinned_tweet_id")
  fields <- check_fields(arg_def(fields,
                                 set_fields(media = NULL,
                                            poll = NULL,
                                            place = NULL)),
                         metrics = NULL)
  expansions_for_fields(expansions, fields)
  if (!is_logical(verbose)) {
    abort("`verbose` must be either `TRUE` or `FALSE`.")
  }
  parsing(parse, expansions, fields)
  data <- c(list(expansions = expansions), fields, ...)
  data <- unlist(prepare_params(data), recursive = FALSE)
  url <- paste0("users/me")

  # Rates from the website app and user limits
  # token <- check_token_v2(token, "pkce")
  # check_scopes_token(token, c("tweet.read", "users.read"))
  req_archive <- endpoint_v2(url, 75/(60*15), c("tweet.read", "users.read"))
  req_final <- httr2::req_url_query(req_archive, !!!data)
  p <- pagination(req_final, 1, 1, verbose)
  if (!parse) {
    return(p)
  }
  parse(p, expansions, fields)
}

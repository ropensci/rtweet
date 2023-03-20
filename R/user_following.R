#' Find which users are being followed.
#'
#' List of users the specified user ID is following.
#' @inheritParams filtered_stream
#' @param ... Other arguments passed to the API.
#' @param n Number of users to query.
#' @param verbose A logical value to provide more information about paginated queries.
#' @param id A user id string.
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/users/follows/api-reference/get-users-id-following>
#' @examples
#' if (FALSE) {
#'   uf <- user_following("1599030512919650304", verbose = TRUE)
#' }
user_following <- function(id, n = 100, expansions = NULL, fields = NULL, ...,
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
  stopifnot(is_n(n))
  if (length(id) != 1 || !is_user_id(id)) {
    abort("Please introduce a single valid user id")
  }
  data <- c(list(expansions = expansions), fields, ...)
  data <- unlist(prepare_params(data), recursive = FALSE)
  url <- paste0("users/", id, "/following")

  max_results <- check_interval(n, 1, formals()$n)
  n_pages <- ceiling(n / max_results)
  data <- c(max_results = max_results, data)

  # Rates from the website app and user limits
  token <- check_token_v2(token, c("bearer", "pkce"))
  check_scopes_token(token, c("tweet.read", "users.read", "follows.read"))
  rate <- check_rate(token, 15/(60*15), 15/(60*15))
  req_archive <- endpoint_v2(token, url, rate)
  req_final <- httr2::req_url_query(req_archive, !!!data)
  p <- pagination(req_final, n_pages, n, verbose)
  if (!parse) {
    return(p)
  }
  parse(p, expansions, fields)
}

#' Retrieve user bookmarks
#'
#' Collects the 800 most recent bookmarked tweets of a user.
#' @note This endpoint requires a OAuth2.0 authentication, with tweet.read, users.read and bookmark.read permissions.
#' @param id Twitter user id: character string identifying your account.
#' @param n Number of tweets to retrieve.
#' @param ... Other arguments passed down to the API.
#' @param verbose A logical value
#' @param token This endpoint only accept a OAuth2.0 authentication (can be
#' created via [rtweet_oauth2()]).
#' @inheritParams sample_stream
#' @seealso [rtweet_oauth2()]
#' @export
#' @returns The tweets.
#' @references <https://developer.twitter.com/en/docs/twitter-api/tweets/bookmarks/api-reference/get-users-id-bookmarks>
#' @examples
#' if (FALSE) {
#'    # Requires token_oa2
#'    ub <- user_bookmarks("123456789", parse = FALSE, n = Inf, token = token_oa2)
#' }
user_bookmarks <- function(id, n = 100, ..., expansions = NA, fields = NA,
                           parse = TRUE, token = NULL, verbose = FALSE) {
  parsing(parse)
  if (!is_logical(verbose)) {
    abort("`verbose` must be either `TRUE` or `FALSE`.")
  }
  max_results <- check_interval(n, 1, formals()$n)
  n_pages <- ceiling(n / max_results)
  expansions <- check_expansions(arg_def(expansions, set_expansions()))
  fields <- check_fields(arg_def(fields, set_fields()), metrics = NULL)
  expansions_for_fields(expansions, fields)
  data <- c(list(expansions = expansions), fields, max_results = max_results, ...)
  data <- unlist(prepare_params(data), recursive = FALSE)
  url <- paste0("users/", id,"/bookmarks")
  token <- check_token_v2(token, mechanism = "pkce")
  check_scopes(get_scopes(token), c("tweet.read", "users.read", "bookmark.read"))
  req_bookmarks <- endpoint_v2(token, url, 180 / (60*15))
  req_final <- httr2::req_url_query(req_bookmarks, !!!data)
  p <- pagination(req_final, n_pages, n, verbose)
  if (!parse) {
    return(p)
  }
  parse(p, expansions, fields)
}

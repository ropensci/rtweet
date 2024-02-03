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
#' @seealso [rtweet_oauth2()], [user_self()]
#' @export
#' @returns A data.frame with the user information of who is following the list:
#' edit_history_tweet_ids, id and text.
#' Other information depends on the `expansions` and `fields` requested.
#' Accepted values are:
#' - Expansions: `set_expansions(list = NULL)`.
#' - Fields: `set_fields(list = NULL)`.
#' @references <https://developer.twitter.com/en/docs/twitter-api/tweets/bookmarks/api-reference/get-users-id-bookmarks>
#' @examples
#' if (FALSE) {
#'    # Requires token_oa2
#'    ub <- user_bookmarks(user_self()$id, parse = FALSE, n = Inf, token = token_oa2)
#' }
user_bookmarks <- function(id, n = 100, ..., expansions = NULL, fields = NULL,
                           parse = TRUE, token = NULL, verbose = FALSE) {
  parsing(parse, expansions, fields)
  if (!is_logical(verbose)) {
    abort("`verbose` must be either `TRUE` or `FALSE`.")
  }
  max_results <- check_interval(n, 1, formals()$n)
  n_pages <- ceiling(n / max_results)
  expansions <- check_expansions(arg_def(expansions, set_expansions(list = NULL)))
  fields <- check_fields(arg_def(fields, set_fields(list = NULL)), metrics = NULL)
  expansions_for_fields(expansions, fields)
  data <- c(list(expansions = expansions), fields, max_results = max_results, ...)
  data <- unlist(prepare_params(data), recursive = FALSE)
  if (!is_user_id(id)) {
    abort(c("Please introduce a valid user id.",
            "i" = "Have you introduced your user name instead of your code number 123456789?"))
  }
  url <- paste0("users/", id,"/bookmarks")
  req_bookmarks <- endpoint_v2(url, 180 / (60*15),
                               c("tweet.read", "users.read", "bookmark.read"))
  req_final <- httr2::req_url_query(req_bookmarks, !!!data)
  p <- pagination(req_final, n_pages, n, verbose)
  if (!parse) {
    return(p)
  }
  parse(p, expansions, fields)
}

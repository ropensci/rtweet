#' Search in the Twitter archive
#'
#' @inheritParams tweet_retweeted_by
#' @inheritParams tweet_get
#' @param query One query for matching Tweets.
#' @note OAuth2.0 requires tweet.read and users.read permissions.
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-all>
#' @examples
#' if (FALSE) {
#'   sa <- tweet_search_all("#rtweet", parse = FALSE)
#' }
tweet_search_all <- function(query, n = 500, expansions = NULL, fields = NULL,
                           ..., token = NULL, parse = TRUE, verbose = FALSE) {

  expansions <- check_expansions(arg_def(expansions, set_expansions(list = NULL)))
  fields <- check_fields(arg_def(fields, set_fields()), metrics = NULL)
  expansions_for_fields(expansions, fields)
  if (!is_logical(verbose)) {
    abort("`verbose` must be either `TRUE` or `FALSE`.")
  }
  parsing(parse, expansions, fields)
  stopifnot(is_n(n))
  max_results <- check_interval(n, 10, formals()$n)
  n_pages <- ceiling(n / max_results)
  data <- c(list(expansions = expansions), fields, ...)
  data <- unlist(prepare_params(data), recursive = FALSE)
  data <- c(query = query, max_results = max_results, data)
  data <- data[data != ""]
  # Rates from the website app and user limits
  token <- check_token_v2(token)
  rate <- max(300/(60*15), 1)
  req_archive <- endpoint_v2(token, "tweets/search/all", rate)
  req_final <- httr2::req_url_query(req_archive, !!!data)
  p <- pagination(req_final, n_pages, n, verbose)
  if (!parse) {
    return(p)
  }
  parse(p, expansions, fields)
}

#' Search recent tweets
#'
#' Look up tweets from the last seven days that match a search query.
#' @inheritParams tweet_retweeted_by
#' @inheritParams tweet_get
#' @param query One query for matching Tweets.
#' @export
#' @note OAuth2.0 requires tweet.read and users.read permissions.
#' @references <https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-recent>
#' @examples
#' if (FALSE) {
#'   sr <- tweet_search_recent("#rtweet", sort_order = "relevancy", parse = FALSE)
#' }
tweet_search_recent <- function(query, n = 100, expansions = NULL, fields = NULL,
                          ..., token = NULL, parse = TRUE, verbose = FALSE) {
  expansions <- check_expansions(arg_def(expansions, set_expansions(list = NULL)))
  fields <- check_fields(arg_def(fields, set_fields()), metrics = NULL)
  expansions_for_fields(expansions, fields)
  if (!is_logical(verbose)) {
    abort("`verbose` must be either `TRUE` or `FALSE`.")
  }
  parsing(parse, expansions, fields)
  stopifnot(is_n(n))
  max_results <- check_interval(n, 10, formals()$n)
  n_pages <- ceiling(n / max_results)
  data <- c(list(expansions = expansions), fields, ...)
  data <- unlist(prepare_params(data), recursive = FALSE)
  data <- c(query = query, max_results = max_results, data)
  data <- data[data != ""]
  # Rates from the website app and user limits
  token <- check_token_v2(token, c("bearer", "pkce"))
  check_scopes_token(token, c("tweet.read", "users.read"))
  rate <- check_rate(token, 450/(15*60), 180/(15*60))
  req_archive <- endpoint_v2(token, "tweets/search/recent", rate)
  req_final <- httr2::req_url_query(req_archive, !!!data)
  p <- pagination(req_final, n_pages, n, verbose)
  if (!parse) {
    return(p)
  }
  parse(p, expansions, fields)
}

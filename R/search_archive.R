#' @references <https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-all>
#' @examples
#' search_archive("#rtweet", parse = FALSE)
search_archive <- function(query, max_results = 500, expansions = NA, fields = NA,
                           ..., token = NULL, parse = TRUE) {
  fields <- check_fields(fields, metrics.fields = NULL)
  expansions <- check_expansions(expansions, tweet_expansions())

  parsing(parse)
  max_results <- check_interval(max_results, 10, 500)
  data <- c(expansions, fields, ...)
  data <- unlist(prepare_params(data), recursive = FALSE)
  data <- c(query = query, max_results = max_results, data)
  # Rates from the website app and user limits
  rate <- max(300/(60*15), 1)
  token <- check_token_v2(token)
  req_archive <- endpoint_v2(token, "tweets/search/all", rate)
  req_final <- httr2::req_url_query(req_archive, !!!data)
  resp <- httr2::req_perform(req_final)
  httr2::resp_body_json(resp)
}

#' @references <https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-recent>
#' @examples
#' search_recent("#rtweet", parse = FALSE)
search_recent <- function(query, max_results = 100, expansions = NA, fields = NA,
                          ..., token = NULL, parse = TRUE) {
    fields <- check_fields(fields, metrics.fields = NULL)
    expansions <- check_expansions(expansions, tweet_expansions())
    parsing(parse)
    max_results <- check_interval(max_results, 10, 100)
    data <- c(expansions, fields, ...)
    data <- unlist(prepare_params(data), recursive = FALSE)
    data <- c(query = query, max_results = max_results, data)
    # Rates from the website app and user limits
    rate <- max(180/(15*60), 450/(15*60))
    req_archive <- endpoint_v2(token, "tweets/search/recent", rate)
    req_final <- httr2::req_url_query(req_archive, !!!data)
    resp <- httr2::req_perform(req_final)
    httr2::resp_body_json(resp)
}

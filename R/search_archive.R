#' Search in the Twitter archive
#' @inheritParams filtered_stream
#' @inheritParams retweeted_by
#'
#' @references <https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-all>
#' @examples
#' sa <- search_archive("#rtweet", parse = FALSE)
search_archive <- function(query, n = 500, expansions = NULL, fields = NULL,
                           ..., token = NULL, parse = TRUE, verbose = TRUE) {
  if (is.logical(expansions) && !isFALSE(expansions)) {
    expansions <- set_expansions()
  } else {
    expansions <- check_expansions(expansions)
  }

  if (is.logical(fields) && !isFALSE(fields)) {
    fields <- set_fields()
  } else {
    fields <- check_fields(fields, metrics = NULL)
  }

  parsing(parse)
  stopifnot(is_n(n))
  max_results <- check_interval(n, 10, 500)
  n_pages <- n %/% max_results
  data <- c(expansions, fields, ...)
  data <- unlist(prepare_params(data), recursive = FALSE)
  data <- c(query = query, max_results = max_results, data)
  # Rates from the website app and user limits
  token <- check_token_v2(token)
  rate <- max(300/(60*15), 1)
  req_archive <- endpoint_v2(token, "tweets/search/all", rate)
  req_final <- httr2::req_url_query(req_archive, !!!data)
  p <- pagination(req_final, n_pages, verbose)
  if (!parse) {
    return(p)
  }
}

#' Search recent tweets
#' @inheritParams search_archive
#' @references <https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-recent>
#' @examples
#' sr <- search_recent("#rtweet", parse = FALSE)
search_recent <- function(query, n = 100, expansions = NULL, fields = NULL,
                          ..., token = NULL, parse = TRUE, verbose = TRUE) {
  if (is.logical(expansions) && !isFALSE(expansions)) {
    expansions <- set_expansions()
  } else {
    expansions <- check_expansions(expansions)
  }

  if (is.logical(fields) && !isFALSE(fields)) {
    fields <- set_fields()
  } else {
    fields <- check_fields(fields, metrics = NULL)
  }
  parsing(parse)
  stopifnot(is_n(n))
  max_results <- check_interval(n, 10, 100)
  n_pages <- n %/% max_results
  data <- c(expansions, fields, ...)
  data <- unlist(prepare_params(data), recursive = FALSE)
  data <- c(query = query, max_results = max_results, data)
  # Rates from the website app and user limits
  token <- check_token_v2(token, c("bearer", "pkce"))
  rate <- check_rate(token, 450/(15*60), 180/(15*60))
  req_archive <- endpoint_v2(token, "tweets/search/recent", rate)
  req_final <- httr2::req_url_query(req_archive, !!!data)
  p <- pagination(req_final, n_pages, verbose)
  if (!parse) {
    return(p)
  }

}

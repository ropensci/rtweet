#' Count tweets
#'
#' @inheritParams tweet_search_recent
#' @returns The number of tweets for a given granularity
#' @references <https://developer.twitter.com/en/docs/twitter-api/tweets/counts/api-reference/get-tweets-counts-all>
#' @references <https://developer.twitter.com/en/docs/twitter-api/tweets/counts/api-reference/get-tweets-counts-recent>
#' @examples
#' if (FALSE) {
#'   tcr <- tweet_counts_recent(query = "#rtweet", parse = FALSE)
#'   tca <- tweet_counts_all(query = "#rtweet", parse = FALSE)
#' }
#' To paginate the next_token is passed as an argument to the query!
tweet_counts_recent <- function(query, ..., token = NULL, parse = TRUE,
                                verbose = FALSE) {
  if (!is_logical(verbose)) {
    abort("`verbose` must be either `TRUE` or `FALSE`.")
  }
  parsing(parse)
  data <- list(...)
  if (!is.null(data$granularity)) {
    data$granularity <- match.arg(data$granularity, c("minute", "hour", "day"))
  }
  data <- unlist(prepare_params(data), recursive = FALSE)
  data <- c(query = query, data)
  # Rates from the website app and user limits
  token <- check_token_v2(token, "bearer")
  req_archive <- endpoint_v2(token, "tweets/counts/recent", 300/(15*60))
  req_final <- httr2::req_url_query(req_archive, !!!data)
  p <- pagination(req_final, Inf, Inf, verbose)
  if (!parse) {
    return(p)
  }
}

tweet_counts_all <- function(query, ..., token = NULL, parse = TRUE,
                             verbose = FALSE) {
  if (!is_logical(verbose)) {
    abort("`verbose` must be either `TRUE` or `FALSE`.")
  }
  parsing(parse)
  data <- list(...)
  if (!is.null(data$granularity)) {
    data$granularity <- match.arg(data$granularity, c("minute", "hour", "day"))
  }
  data <- unlist(prepare_params(data), recursive = FALSE)
  data <- c(query = query, data)
  # Rates from the website app and user limits
  token <- check_token_v2(token, "bearer")
  req_archive <- endpoint_v2(token, "tweets/counts/all", 300/(15*60))
  req_final <- httr2::req_url_query(req_archive, !!!data)
  p <- pagination(req_final, Inf, Inf, verbose)
  if (!parse) {
    return(p)
  }
}
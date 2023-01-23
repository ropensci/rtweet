#' Retweets
#'
#' Looks up who have retweeted a given id.
#' @inheritParams stream_filter
#' @inheritParams get_retweeters
#' @export
#' @references https://developer.twitter.com/en/docs/twitter-api/tweets/retweets/api-reference/get-tweets-id-retweeted_by
#' @examples
#' rb <- retweeted_by("567053242429734913", parse = FALSE)
retweeted_by <- function(id, n = 100, expansions = NA, fields = NA, ...,
                      token = NULL, parse = TRUE) {
  fields <- check_fields(fields, metrics = NULL)
  expansions <- check_expansions(expansions)
  parsing(parse)
  data <- c(expansions, fields, ...)
  data <- unlist(prepare_params(data), recursive = FALSE)
  if (length(id) != 1) {
    abort("Only one tweet id per call is allowed")
  }
  url <- paste0("tweets/", id, "/retweeted_by")

  max_results <- check_interval(n, 10, 100)
  data <- c(max_results = max_results, data)

  # Rates from the website app and user limits
  rate <- max(75/(60*15), 75/(60*15))
  token <- check_token_v2(token)
  req_archive <- endpoint_v2(token, url, rate)
  req_final <- httr2::req_url_query(req_archive, !!!data)
  resp <- httr2::req_perform(req_final)
  httr2::resp_body_json(resp)
}

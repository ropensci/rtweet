#' Retweets
#'
#' Looks up who have retweeted a given id.
#' @inheritParams filtered_stream
#' @inheritParams get_retweeters
#' @param ... Other arguments passed to the API.
#' @param verbose A logical value to provide more information about paginated queries.
#' @param id A tweet id string.
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/tweets/retweets/api-reference/get-tweets-id-retweeted_by>
#' @examples
#' rb <- retweeted_by("567053242429734913", parse = FALSE)
retweeted_by <- function(id, n = 100, expansions = NULL, fields = NULL, ...,
                      token = NULL, parse = TRUE, verbose = TRUE) {
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
  if (length(id) != 1 || !is_id(id)) {
    abort("Please introduce a single valid id")
  }
  data <- c(expansions, fields, ...)
  data <- unlist(prepare_params(data), recursive = FALSE)
  url <- paste0("tweets/", id, "/retweeted_by")

  max_results <- check_interval(n, 10, 100)
  n_pages <- n %/% max_results
  data <- c(max_results = max_results, data)

  # Rates from the website app and user limits
  token <- check_token_v2(token, c("bearer", "pkce"))
  rate <- check_rate(token, 75/(60*15), 75/(60*15))
  req_archive <- endpoint_v2(token, url, rate)
  req_final <- httr2::req_url_query(req_archive, !!!data)
  p <- pagination(req_final, n_pages, verbose = TRUE)
  if (!parse) {
    return(p)
  }
}

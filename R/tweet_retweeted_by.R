#' Tweet retweeted by
#'
#' Looks up who have retweeted a given tweet.
#' @inheritParams filtered_stream
#' @param ... Other arguments passed to the API.
#' @param n Number of tweets to query.
#' @param verbose A logical value to provide more information about the
#' paginated queries (if any) and to store the data of each page.
#' @param ids A tweet id string.
#' @returns A data.frame with the user information of who retweeted it:
#' id, name, and username.
#' Other information depends on the `expansions` and `fields` requested.
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/tweets/retweets/api-reference/get-tweets-id-retweeted_by>
#' @examples
#' if (FALSE) {
#'   rb <- tweet_retweeted_by("567053242429734913")
#' }
tweet_retweeted_by <- function(ids, n = 100, expansions = NULL, fields = NULL, ...,
                      token = NULL, parse = TRUE, verbose = FALSE) {

  expansions <- check_expansions(arg_def(expansions, set_expansions(list = NULL)))
  fields <- check_fields(arg_def(fields, set_fields()), metrics = NULL)
  expansions_for_fields(expansions, fields)
  if (!is_logical(verbose)) {
    abort("`verbose` must be either `TRUE` or `FALSE`.")
  }
  parsing(parse, expansions, fields)
  stopifnot(is_n(n))
  if (length(ids) != 1 || !is_id(ids)) {
    abort("Please introduce a single valid id")
  }
  data <- c(list(expansions = expansions), fields, ...)
  data <- unlist(prepare_params(data), recursive = FALSE)
  url <- paste0("tweets/", ids, "/retweeted_by")

  max_results <- check_interval(n, 10, formals()$n)
  n_pages <- ceiling(n / max_results)
  data <- c(max_results = max_results, data)

  # Rates from the website app and user limits
  req_archive <- endpoint_v2(url, 75/(60*15), c("tweet.read", "users.read"))
  req_final <- httr2::req_url_query(req_archive, !!!data)
  p <- pagination(req_final, n_pages, n, verbose)
  if (!parse) {
    return(p)
  }
  parse(p, expansions, fields)
}

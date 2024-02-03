#' Liking users
#'
#' Looks up who have liked a given tweet.
#' @inheritParams filtered_stream
#' @param ... Other arguments passed to the API.
#' @param n Number of tweets to query.
#' @param verbose A logical value to provide more information about paginated queries.
#' @param id A tweet id string.
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/tweets/likes/api-reference/get-tweets-id-liking_users>
#' @examples
#' if (FALSE) {
#'   tlu <- tweet_liking_users("567053242429734913", n = Inf, verbose = TRUE)
#' }
tweet_liking_users <- function(id, n = 100, expansions = NULL, fields = NULL, ...,
                               token = NULL, parse = TRUE, verbose = FALSE) {

  expansions <- check_expansions(arg_def(expansions, "pinned_tweet_id"),
                                 "pinned_tweet_id")
  fields <- check_fields(arg_def(fields, set_fields()),
                         metrics = NULL,
                         place = NULL,
                         media = NULL,
                         poll = NULL)
  expansions_for_fields(expansions, fields)
  if (!is_logical(verbose)) {
    abort("`verbose` must be either `TRUE` or `FALSE`.")
  }
  parsing(parse, expansions, fields)
  stopifnot(is_n(n))
  if (length(id) != 1 || !is_id(id)) {
    abort("Please introduce a single valid id")
  }
  data <- c(list(expansions = expansions), fields, ...)
  data <- unlist(prepare_params(data), recursive = FALSE)
  url <- paste0("tweets/", id, "/liking_users")

  max_results <- check_interval(n, 1, formals()$n)
  n_pages <- ceiling(n / max_results)
  data <- c(max_results = max_results, data)

  # Rates from the website app and user limits
  req_archive <- endpoint_v2(url, 75/(60*15), c("tweet.read", "users.read", "like.read"))
  req_final <- httr2::req_url_query(req_archive, !!!data)
  p <- pagination(req_final, n_pages, n, verbose)
  if (!parse) {
    return(p)
  }
  parse(p, expansions, fields)
}

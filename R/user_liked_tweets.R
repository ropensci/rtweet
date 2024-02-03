#' Liked tweets from a user
#'
#' Looks up tweets liked by a user.
#' @inheritParams filtered_stream
#' @param ... Other arguments passed to the API.
#' @param n Number of tweets to query.
#' @param verbose A logical value to provide more information about paginated queries.
#' @param id A tweet id string.
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/tweets/likes/api-reference/get-users-id-liked_tweets>
#' @examples
#' if (FALSE) {
#'   ult <- user_liked_tweets("1599030512919650304", verbose = TRUE)
#' }
user_liked_tweets <- function(id, n = 100, expansions = NULL, fields = NULL, ...,
                               token = NULL, parse = TRUE, verbose = FALSE) {

  expansions <- check_expansions(arg_def(expansions, set_expansions(list = NULL)))
  fields <- check_fields(arg_def(fields, set_fields()), metrics = NULL)
  expansions_for_fields(expansions, fields)
  if (!is_logical(verbose)) {
    abort("`verbose` must be either `TRUE` or `FALSE`.")
  }
  parsing(parse, expansions, fields)
  stopifnot(is_n(n))
  if (length(id) != 1 || !is_user_id(id)) {
    abort("Please introduce a single valid user id")
  }
  data <- c(list(expansions = expansions), fields, ...)
  data <- unlist(prepare_params(data), recursive = FALSE)
  url <- paste0("users/", id, "/liked_tweets")

  max_results <- check_interval(n, 10, formals()$n)
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

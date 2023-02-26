#' Tweets from a user
#'
#' Looks up tweets posted by a user.
#' @inheritParams filtered_stream
#' @param ... Other arguments passed to the API.
#' @param n Number of tweets to query.
#' @param verbose A logical value to provide more information about paginated queries.
#' @param id A user id string.
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/tweets/timelines/api-reference/get-users-id-tweets>
#' @examples
#' if (FALSE) {
#'   ut <- user_tweets("1599030512919650304", verbose = TRUE)
#' }
user_tweets <- function(id, n = 100, expansions = NULL, fields = NULL, ...,
                              token = NULL, parse = TRUE, verbose = FALSE) {

  expansions <- check_expansions(arg_def(expansions, set_expansions()))
  fields <- check_fields(arg_def(fields, set_fields()), metrics = NULL)
  expansions_for_fields(expansions, fields)
  if (!is_logical(verbose)) {
    abort("`verbose` must be either `TRUE` or `FALSE`.")
  }
  parsing(parse)
  stopifnot(is_n(n))
  if (length(id) != 1 || !is_user_id(id)) {
    abort("Please introduce a single valid user id")
  }
  data <- c(list(expansions = expansions), fields, ...)
  data <- unlist(prepare_params(data), recursive = FALSE)
  url <- paste0("users/", id, "/tweets")

  max_results <- check_interval(n, 5, formals()$n)
  n_pages <- ceiling(n / max_results)
  data <- c(max_results = max_results, data)

  # Rates from the website app and user limits
  token <- check_token_v2(token, c("bearer", "pkce"))
  rate <- check_rate(token, 1500/(60*15), 900/(60*15))
  req_archive <- endpoint_v2(token, url, rate)
  req_final <- httr2::req_url_query(req_archive, !!!data)
  p <- pagination(req_final, n_pages, n, verbose)
  if (!parse) {
    return(p)
  }
  parse(p, expansions, fields)
}
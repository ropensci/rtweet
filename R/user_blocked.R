#' Find users blocked.
#'
#' List of users that are blocked.
#' @inheritParams filtered_stream
#' @param ... Other arguments passed to the API.
#' @param n Number of tweets to query.
#' @param verbose A logical value to provide more information about paginated queries.
#' @param id A user id string.
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/users/blocks/api-reference/get-users-blocking>
#' @examples
#' if (FALSE) {
#'   uf <- user_blocked("1599030512919650304", verbose = TRUE)
#' }
user_blocked <- function(id, n = 1000, expansions = NULL, fields = NULL, ...,
                           token = NULL, parse = TRUE, verbose = FALSE) {

  expansions <- check_expansions(arg_def(expansions, "pinned_tweet_id"),
                                 "pinned_tweet_id")
  fields <- check_fields(arg_def(fields, set_fields(media = NULL,
                                                    poll = NULL,
                                                    place = NULL)),
                         metrics = NULL, place = NULL, poll = NULL, media = NULL)
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
  url <- paste0("users/", id, "/blocking")

  max_results <- check_interval(n, 1, formals()$n)
  n_pages <- ceiling(n / max_results)
  data <- c(max_results = max_results, data)

  # Rates from the website app and user limits
  req_archive <- endpoint_v2(url, 15/(60*15), c("tweet.read", "users.read", "block.read"))
  req_final <- httr2::req_url_query(req_archive, !!!data)
  p <- pagination(req_final, n_pages, n, verbose)
  if (!parse) {
    return(p)
  }
  parse(p, expansions, fields)
}

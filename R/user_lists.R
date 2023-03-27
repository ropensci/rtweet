#' Search users by username
#'
#' Looks up users by their username.
#' @inheritParams filtered_stream
#' @inheritParams user_following
#' @param ids A user name string or up to 100.
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/lists/list-lookup/api-reference/get-users-id-owned_lists>
#' @examples
#' if (FALSE) {
#'   ul <- user_lists("1051050384")
#' }
user_lists <- function(ids, n = 100, expansions = NULL, fields = NULL, ...,
                       token = NULL, parse = TRUE, verbose = FALSE) {

  expansions <- check_expansions(arg_def(expansions, "owner_id"), "owner_id")
  fields <- check_fields(arg_def(fields, list_fields),
                         metrics = NULL, place = NULL, poll = NULL,
                         media = NULL, user = NULL, tweet = NULL)
  expansions_for_fields(expansions, fields)
  if (!is_logical(verbose)) {
    abort("`verbose` must be either `TRUE` or `FALSE`.")
  }
  parsing(parse, expansions, fields)
  if (length(ids) != 1 || !is_user_id(ids) && length(ids) == 0) {
    abort("Please introduce at least a valid user id")
  }

  data <- c(list(expansions = expansions), fields, ...)
  url <- paste0("users/", ids, "/owned_lists")
  max_results <- check_interval(n, 1, formals()$n)
  n_pages <- ceiling(n / max_results)
  data <- c(max_results = max_results, data)

  data <- unlist(prepare_params(data), recursive = FALSE)
  data <- data[data != ""]

  # Rates from the website app and user limits
  token <- check_token_v2(token, c("bearer", "pkce"))
  check_scopes_token(token, c("tweet.read", "users.read", "list.read"))
  rate <- check_rate(token, 15/(60*15), 15/(60*15))
  req_archive <- endpoint_v2(token, url, rate)
  req_final <- httr2::req_url_query(req_archive, !!!data)
  p <- pagination(req_final, n_pages, n, verbose)
  if (!parse) {
    return(p)
  }
  parse(p, expansions, fields)
}

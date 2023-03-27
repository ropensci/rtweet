
#' Lists a specified user follows
#'
#' Looks up lists a user follows.
#' @inheritParams list_get
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/lists/list-follows/api-reference/get-lists-id-followers>
#' @examples
#' if (FALSE) {
#'   ulf <- user_list_follows("1051050384")
#' }
user_list_follows <- function(ids, n = 100, expansions = NULL, fields = NULL, ...,
                           token = NULL, parse = TRUE, verbose = FALSE) {

  expansions <- check_expansions(arg_def(expansions, "owner_id"),
                                 "owner_id")
  fields <- check_fields(
    arg_def(fields,
            set_fields(place = NULL, poll = NULL, media = NULL, tweet = NULL)),
    metrics = NULL, place = NULL, poll = NULL, media = NULL, tweet = NULL)
  expansions_for_fields(expansions, fields)
  if (!is_logical(verbose)) {
    abort("`verbose` must be either `TRUE` or `FALSE`.")
  }
  parsing(parse, expansions, fields)
  if (length(ids) != 1 || !is_user_id(ids) && length(ids) == 0) {
    abort("Please introduce at least a valid list id")
  }

  data <- c(list(expansions = expansions), fields, ...)
  url <- paste0("users/", ids, "/followed_lists")

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

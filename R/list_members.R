
#' List of members from a specified List
#'
#' Looks up the users of a list.
#' @inheritParams list_get
#' @export
#' @returns A data.frame with the user information of who is included in the list:
#' id, name, and username.
#'
#' Other information depends on the `expansions` and `fields` requested.
#' Accepted values are:
#' - Expansions: `set_expansions(tweet = NULL, list = NULL)`
#' - Fields: `set_fields(place = NULL, poll = NULL, media = NULL, list = NULL)`.
#' @references <https://developer.twitter.com/en/docs/twitter-api/lists/list-lookup/api-reference/get-lists-id>
#' @examples
#' if (FALSE) {
#'   lm <- list_members("1306285118877831168")
#' }
list_members <- function(ids, n = 100, expansions = NULL, fields = NULL, ...,
                     token = NULL, parse = TRUE, verbose = FALSE) {

  expansions <- check_expansions(arg_def(expansions, user_expansions()),
                                 user_expansions())
  fields <- check_fields(
    arg_def(fields,
            set_fields(place = NULL, poll = NULL, media = NULL, list = NULL)),
    metrics = NULL, place = NULL, poll = NULL, media = NULL, list = NULL)
  expansions_for_fields(expansions, fields)
  if (!is_logical(verbose)) {
    abort("`verbose` must be either `TRUE` or `FALSE`.")
  }
  parsing(parse, expansions, fields)
  if (length(ids) != 1 || !is_list_id(ids) && length(ids) == 0) {
    abort("Please introduce at least a valid list id")
  }

  data <- c(list(expansions = expansions), fields, ...)
  url <- paste0("lists/", ids, "/members")

  max_results <- check_interval(n, 1, formals()$n)
  n_pages <- ceiling(n / max_results)
  data <- c(max_results = max_results, data)

  data <- unlist(prepare_params(data), recursive = FALSE)
  data <- data[data != ""]

  # Rates from the website app and user limits
  token <- check_token_v2(token, c("bearer", "pkce"))
  check_scopes_token(token, c("tweet.read", "users.read", "list.read"))
  rate <- check_rate(token, 900/(60*15), 900/(60*15))
  req_archive <- endpoint_v2(token, url, rate)
  req_final <- httr2::req_url_query(req_archive, !!!data)
  p <- pagination(req_final, n_pages, n, verbose)
  if (!parse) {
    return(p)
  }
  parse(p, expansions, fields)
}

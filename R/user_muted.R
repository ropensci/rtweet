
#' List muted users
#'
#' Looks up the muted users.
#' @inheritParams list_get
#' @export
#' @returns A data.frame with the user information of who is following the list:
#' id, name, and username.
#' Other information depends on the `expansions` and `fields` requested.
#' Accepted values are:
#' - Expansions: `set_expansions(tweet = NULL, list = NULL)`.
#' - Fields: `set_fields(media = NULL, poll = NULL, place = NULL, list = NULL)`.
#' @references <https://developer.twitter.com/en/docs/twitter-api/users/mutes/api-reference/get-users-muting>
#' @seealso [user_self()]
#' @examples
#' if (FALSE) {
#'   um <- user_muted(user_self()$id)
#' }
user_muted <- function(ids, n = 1000, expansions = NULL, fields = NULL, ...,
                           token = NULL, parse = TRUE, verbose = FALSE) {


  expansions <- check_expansions(
    arg_def(expansions,
            set_expansions(tweet = NULL, list = NULL)),
    set_expansions(tweet = NULL, list = NULL))
  fields <- check_fields(
    arg_def(fields,
            set_fields(place = NULL, poll = NULL, list = NULL)),
    metrics = NULL, place = NULL, poll = NULL, list = NULL)
  expansions_for_fields(expansions, fields)
  if (!is_logical(verbose)) {
    abort("`verbose` must be either `TRUE` or `FALSE`.")
  }
  parsing(parse, expansions, fields)
  if (length(ids) != 1 || !is_list_id(ids) && length(ids) == 0) {
    abort("Please introduce at least a valid list id")
  }

  data <- c(list(expansions = expansions), fields, ...)
  url <- paste0("users/", ids, "/muting")
  max_results <- check_interval(n, 1, formals()$n)
  n_pages <- ceiling(n / max_results)
  data <- c(max_results = max_results, data)

  data <- unlist(prepare_params(data), recursive = FALSE)
  data <- data[data != ""]

  # Rates from the website app and user limits
  req_archive <- endpoint_v2(url, 900/(60*15), c("tweet.read", "users.read", "mute.read"))
  req_final <- httr2::req_url_query(req_archive, !!!data)
  p <- pagination(req_final, n_pages, n, verbose)
  if (!parse) {
    return(p)
  }
  parse(p, expansions, fields)
}

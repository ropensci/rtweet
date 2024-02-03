#' List information
#'
#' Looks up information about a list.
#' @inheritParams filtered_stream
#' @inheritParams user_following
#' @param ids A list id.
#' @export
#' @returns A data.frame with the user information of who is included in the list:
#' id, name, and username.
#'
#' Other information depends on the `expansions` and `fields` requested.
#' Accepted values are:
#' - Expansions: `set_expansions(tweet = NULL, user = NULL)`
#' - Fields: `set_fields(place = NULL, poll = NULL, media = NULL, tweet = NULL)`.
#' @references <https://developer.twitter.com/en/docs/twitter-api/lists/list-lookup/api-reference/get-lists-id>
#' @examples
#' if (FALSE) {
#'   lg <- list_get("1306285118877831168")
#' }
list_get <- function(ids, n = 100, expansions = NULL, fields = NULL, ...,
                       token = NULL, parse = TRUE, verbose = FALSE) {

  expansions <- check_expansions(arg_def(expansions, "owner_id"), "owner_id")
  fields <- check_fields(
    arg_def(fields,
            set_fields(place = NULL, poll = NULL, media = NULL, tweet = NULL)),
    metrics = NULL, place = NULL, poll = NULL, media = NULL, tweet = NULL)
  expansions_for_fields(expansions, fields)
  if (!is_logical(verbose)) {
    abort("`verbose` must be either `TRUE` or `FALSE`.")
  }
  parsing(parse, expansions, fields)
  if (length(ids) != 1 || !is_list_id(ids) && length(ids) == 0) {
    abort("Please introduce at least a valid list id")
  }

  data <- c(list(expansions = expansions), fields, ...)
  url <- paste0("lists/", ids)

  data <- unlist(prepare_params(data), recursive = FALSE)
  data <- data[data != ""]

  # Rates from the website app and user limits
  req_archive <- endpoint_v2(url, 75/(60*15), c("tweet.read", "users.read", "list.read"))
  req_final <- httr2::req_url_query(req_archive, !!!data)
  p <- pagination(req_final, 1, 1, verbose = verbose)
  if (!parse) {
    return(p)
  }
  parse(p, expansions, fields)
}

#' List information
#'
#' Looks up information about a list.
#' @inheritParams filtered_stream
#' @inheritParams user_following
#' @param ids A list id.
#' @export
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
  parsing(parse)
  if (length(ids) != 1 || !is_list_id(ids) && length(ids) == 0) {
    abort("Please introduce at least a valid list id")
  }

  data <- c(list(expansions = expansions), fields, ...)
  url <- paste0("lists/", ids)

  data <- unlist(prepare_params(data), recursive = FALSE)
  data <- data[data != ""]

  # Rates from the website app and user limits
  token <- check_token_v2(token, c("bearer", "pkce"))
  rate <- check_rate(token, 75/(60*15), 75/(60*15))
  req_archive <- endpoint_v2(token, url, rate)
  req_final <- httr2::req_url_query(req_archive, !!!data)
  p <- resp(httr2::req_perform(req_final))
  if (!parse) {
    return(p)
  }
  parse(p, expansions, fields)
}
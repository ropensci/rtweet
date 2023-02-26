
#' List followers of a specified list
#'
#' Looks up the followers of a list.
#' @inheritParams list_get
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/lists/list-follows/api-reference/get-lists-id-followers>
#' @examples
#' if (FALSE) {
#'   lf <- list_followers("1150793074420998146")
#' }
list_followers <- function(ids, n = 100, expansions = NULL, fields = NULL, ...,
                         token = NULL, parse = TRUE, verbose = FALSE) {

  expansions <- check_expansions(arg_def(expansions, "pinned_tweet_id"),
                                 "pinned_tweet_id")
  fields <- check_fields(
    arg_def(fields,
            set_fields(place = NULL, poll = NULL, media = NULL, list = NULL)),
    metrics = NULL, place = NULL, poll = NULL, media = NULL, list = NULL)
  expansions_for_fields(expansions, fields)
  if (!is_logical(verbose)) {
    abort("`verbose` must be either `TRUE` or `FALSE`.")
  }
  parsing(parse)
  if (length(ids) != 1 || !is_list_id(ids) && length(ids) == 0) {
    abort("Please introduce at least a valid list id")
  }

  data <- c(list(expansions = expansions), fields, ...)
  url <- paste0("lists/", ids, "/followers")

  max_results <- check_interval(n, 1, formals()$n)
  n_pages <- ceiling(n / max_results)
  data <- c(max_results = max_results, data)

  data <- unlist(prepare_params(data), recursive = FALSE)
  data <- data[data != ""]

  # Rates from the website app and user limits
  token <- check_token_v2(token, c("bearer", "pkce"))
  rate <- check_rate(token, 900/(60*15), 900/(60*15))
  req_archive <- endpoint_v2(token, url, rate)
  req_final <- httr2::req_url_query(req_archive, !!!data)
  p <- pagination(req_final, n_pages, n, verbose)
  if (!parse) {
    return(p)
  }
  parse(p, expansions, fields)
}
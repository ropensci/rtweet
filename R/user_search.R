#' Search users
#'
#' Looks up users.
#' @inheritParams filtered_stream
#' @param ... Other arguments passed to the API.
#' @param verbose A logical value to provide more information about paginated queries.
#' @param ids A user id string or up to 100.
#' @returns
#' A data.frame with the id, name and username of the accounts.
#' Other information depends on the `expansions` and `fields` requested.
#' Accepted values are:
#' - Expansions: `set_expansions(tweet = NULL, list = NULL)`.
#' - Fields: `set_fields(media = NULL, poll = NULL, place = NULL)`.
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/users/lookup/api-reference/get-users-id>
#' <https://developer.twitter.com/en/docs/twitter-api/users/lookup/api-reference/get-users>
#' @seealso [user_by_username()]
#' @examples
#' if (FALSE) {
#'   us <- user_search(c("1599030512919650304", "2244994945"), verbose = TRUE)
#' }
user_search <- function(ids, expansions = NULL, fields = NULL, ...,
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
  if (!length(ids) > 1 || !is_user_id(ids)) {
    abort("Please introduce at least a valid user id.")
  }
  if (length(ids) > 100) {
    abort("Too many ids provided.")
  }
  if (length(ids) > 1) {
    data <- c(list(ids = ids, expansions = expansions), fields, ...)
    url <- "users"
  } else {
    data <- c(list(expansions = expansions), fields, ...)
    url <- paste0("users/", ids)
  }

  data <- unlist(prepare_params(data), recursive = FALSE)
  data <- data[data != ""]

  # Rates from the website app and user limits
  req_archive <- endpoint_v2(url, 900/(60*15), c("tweet.read", "users.read"))
  req_final <- httr2::req_url_query(req_archive, !!!data)
  p <- pagination(req_final, 1, 1, verbose)
  if (!parse) {
    return(p)
  }
  parse(p, expansions, fields)
}

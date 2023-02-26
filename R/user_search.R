#' Search users
#'
#' Looks up users.
#' @inheritParams filtered_stream
#' @param ... Other arguments passed to the API.
#' @param verbose A logical value to provide more information about paginated queries.
#' @param ids A user id string or up to 100.
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/tweets/timelines/api-reference/get-users-id-mentions>
#' @examples
#' if (FALSE) {
#'   ut <- user_search(c("1599030512919650304", "2244994945"), verbose = TRUE)
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
  parsing(parse)
  if (length(ids) > 100 || !is_user_id(ids) && length(ids) == 0) {
    abort("Please introduce at least a valid user id")
  }
  if (length(ids) > 1) {
    data <- c(list(id = ids, expansions = expansions), fields, ...)
    url <- "users"
  } else {
    data <- c(list(expansions = expansions), fields, ...)
    url <- paste0("users/", ids)
  }
  data <- unlist(prepare_params(data), recursive = FALSE)
  data <- data[data != ""]

  # Rates from the website app and user limits
  token <- check_token_v2(token, c("bearer", "pkce"))
  rate <- check_rate(token, 300/(60*15), 900/(60*15))
  req_archive <- endpoint_v2(token, url, rate)
  req_final <- httr2::req_url_query(req_archive, !!!data)
  resp <- httr2::req_perform(req_final)
  p <- resp(resp)
  if (!parse) {
    return(p)
  }
  if (url == "users/") {
    parse(p, expansions, fields)
  } else {
   list2DF(p$data)
  }
}

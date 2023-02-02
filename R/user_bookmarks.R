#' @references <https://developer.twitter.com/en/docs/twitter-api/tweets/bookmarks/api-reference/get-users-id-bookmarks>
user_bookmarks <- function(id, n = 100, ..., expansions = NA, fields = NA,
                           parse = TRUE, token = NULL) {
  parsing(parse)
  max_results <- check_interval(n, 1, 100)
  n_pages <- n %/% max_results
  if (is.logical(fields) && !isFALSE(fields)) {
    fields <- set_fields()
  } else {
    fields <- check_fields(fields, metrics = NULL)
  }
  if (is.logical(expansions) && !isFALSE(expansions)) {
    expansions <- list(expansions = set_expansions())
  } else {
    expansions <- check_expansions(expansions)
  }
  data <- c(expansions, fields, max_results = max_results, ...)
  data <- unlist(prepare_params(data), recursive = FALSE)
  url <- paste0("users/", id,"/bookmarks")
  token <- check_token_v2(token, mechanism = "pkce")
  check_scopes(get_scopes(token), c("tweet.read", "users.read", "bookmark.read"))
  req_bookmarks <- endpoint_v2(token, url, 180 / (60*15))
  req_final <- httr2::req_url_query(req_bookmarks, !!!data)
  pagination(req_final, n_pages, verbose = TRUE)
}

#' Get subscribers of a specified list.
#'
#' @inheritParams TWIT_paginate_cursor
#' @param list_id required The numerical id of the list.
#' @param slug,owner_user The list name (slug) and owner. 
#' @examples
#'
#' \dontrun{
#'
#' ## get subscribers of new york times politics list
#' rstats <- lists_subscribers(
#'   slug = "new-york-times-politics",
#'   owner_user = "nytpolitics",
#'   n = 1000
#' )
#'
#' }
#'
#' @family lists
#' @family users
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/v1/accounts-and-users/create-manage-lists/api-reference/get-lists-subscribers>
lists_subscribers <- function(list_id = NULL,
                              slug = NULL,
                              owner_user = NULL,
                              n = 20,
                              cursor = "-1",
                              parse = TRUE,
                              token = NULL) {

  params <- lists_params(
    list_id = list_id,
    slug = slug,
    owner_user = owner_user
  )

  r <- TWIT_paginate_cursor(token, "/1.1/lists/subscribers", params, 
    n = n,
    cursor = cursor, 
    page_size = 5000,
    get_id = function(x) x$users$id_str
  )

  if (parse) {
    r <- parse_lists_users(r)
  }
  r
}

parse_lists_users <- function(x) {
  users <- lapply(x, function(x) x$users)
  dfs <- lapply(users, wrangle_into_clean_data, type = "user")
  dfs <- lapply(dfs, tibble::as_tibble)
  do.call("rbind", dfs)
}

#' Get subscribers of a specified list.
#'
#' @inheritParams TWIT_paginate_cursor
#' @inheritParams stream
#' @param list_id required The numerical id of the list.
#' @param slug,owner_user The list name (slug) and owner.
#' @family lists
#' @family users
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/v1/accounts-and-users/create-manage-lists/api-reference/get-lists-subscribers>
lists_subscribers <- function(list_id = NULL,
                              slug = NULL,
                              owner_user = NULL,
                              n = 5000,
                              cursor = "-1",
                              parse = TRUE,
                              retryonratelimit = NULL,
                              verbose = TRUE,
                              token = NULL) {

  params <- lists_params(
    list_id = list_id,
    slug = slug,
    owner_user = owner_user
  )

  r <- TWIT_paginate_cursor(token, "/1.1/lists/subscribers", params,
    n = n,
    cursor = cursor,
    retryonratelimit = retryonratelimit,
    verbose = verbose,
    page_size = if (n >= 5000) 5000 else n,
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
  df <- do.call(rbind, dfs)

  copy_cursor(df, x)
}

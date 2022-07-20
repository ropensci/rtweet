#' Get Twitter list members (users on a given list).
#'
#' @inheritParams TWIT_paginate_cursor
#' @param list_id required The numerical id of the list.
#' @param slug required You can identify a list by its slug instead of
#'   its numerical id. If you decide to do so, note that you'll also
#'   have to specify the list owner using the owner_id or
#'   owner_user parameters.
#' @param owner_user optional The screen name or user ID of the user
#' @param ... Other arguments used as parameters in query composition.
#' @examples
#' if (auth_has_default()) {
#'
#' ## get list members for a list of rstats experts using list_id
#' (rstats <- lists_members("785434502382383105"))
#'
#' ## get list members for an rstats list using list topic slug
#' ## list owner's screen name
#' rstats <- lists_members(slug = "rstats", owner_user = "scultrera")
#' rstats
#'
#' }
#'
#' @family lists
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/v1/accounts-and-users/create-manage-lists/api-reference/get-lists-members>
lists_members <- function(list_id = NULL,
                          slug = NULL,
                          owner_user = NULL,
                          n = 5000,
                          cursor = "-1",
                          token = NULL,
                          retryonratelimit = NULL,
                          verbose = TRUE,
                          parse = TRUE,
                          ...) {
  params <- lists_params(
    list_id = list_id,
    slug = slug,
    owner_user = owner_user
  )
  r <- TWIT_paginate_cursor(token, "/1.1/lists/members", params,
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

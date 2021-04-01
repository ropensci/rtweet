#' Get Twitter list members (users on a given list).
#'
#' @param list_id required The numerical id of the list.
#' @param slug required You can identify a list by its slug instead of
#'   its numerical id. If you decide to do so, note that you'll also
#'   have to specify the list owner using the owner_id or
#'   owner_user parameters.
#' @param owner_user optional The screen name or user ID of the user
#' @inheritParams TWIT_paginate_cursor
#' @inheritParams lookup_users
#' @param ... Other arguments used as parameters in query composition.
#' @return Either a nested list (if parsed) or an HTTP response object.
#' @examples
#' \dontrun{
#'
#' ## get list members for a list of polling experts using list_id
#' (pollsters <- lists_members("105140588"))
#'
#' ## get list members of cspan's senators list
#' sens <- lists_members(slug = "senators", owner_user = "cspan")
#' sens
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
                          parse = TRUE,
                          ...) {
  params <- lists_params(
    list_id = list_id,
    slug = slug,
    owner_user = owner_user
  )
  r <- TWIT_paginate_cursor(token, "/1.1/lists/members", params,
    n = n,
    page_size = 200,
    cursor = cursor,
    get_id = function(x) x$users$id_str
  )
  
  if (parse) {
    r <- parse_lists_users(r)
  }
  r
}

#' Get Twitter list members (users on a given list).
#'
#' @param list_id required The numerical id of the list.
#' @param slug required You can identify a list by its slug instead of
#'   its numerical id. If you decide to do so, note that you'll also
#'   have to specify the list owner using the owner_id or
#'   owner_user parameters.
#' @param owner_user optional The screen name or user ID of the user
#'   who owns the list being requested by a slug.
#' @param n Specifies the number of results to return
#'   per page (see cursor below). For `list_memberships()`, the default and
#'   max is 200 per page. Twitter technically allows up to 1,000 per page,
#'   but above 200 frequently results in an over capacity error.
#'   For `lists_members()`, the default, and max number
#'   of users per list, is 5,000.
#' @param cursor optional Breaks the results into pages. Provide a
#'   value of -1 to begin paging. Provide values as returned in the
#'   response body's next_cursor and previous_cursor attributes to
#'   page back and forth in the list.
#' @inheritParams lookup_users
#' @param ... Other arguments used as parameters in query composition.
#' @return Either a nested list (if parsed) or an HTTP response object.
#' @examples
#' if (auth_has_default()) {
#'   # You can use a list id:
#'   pollsters <- lists_members("105140588")
#'   pollsters
#'   
#'   # But in most case it'll be easier to use a user & list name
#'   senators <- lists_members(owner_user = "cspan", slug = "senators")
#'   senators
#' }
#' @family lists
#' @rdname lists_members
#' @export
lists_members <- function(list_id = NULL,
                          slug = NULL,
                          owner_user = NULL,
                          n = 5000,
                          cursor = "-1",
                          token = NULL,
                          parse = TRUE,
                          ...) {
  stopifnot(is.numeric(n))
  if (n > 5000) {
    warning("maximum number of list users it 5,000")
    n <- 5000
  }
  
  params <- lists_params(
    list_id = list_id,
    slug = slug,
    owner_user = owner_user,
    count = n,
    cursor = cursor
  )
  r <- TWIT_get(token, "/1.1/lists/members", params, parse = parse)
  
  if (parse) {
    r <- as_lists_members(r)
    r <- as.data.frame(r)
  }
  r
}

#' Get subscribers of a specified list.
#'
#' @param list_id required The numerical id of the list.
#' @param slug required You can identify a list by its slug instead of
#'   its numerical id. If you decide to do so, note that you'll also
#'   have to specify the list owner using the owner_id or
#'   owner_user parameters.
#' @param owner_user optional The screen name or user ID of the user
#'   who owns the list being requested by a slug.
#' @param n optional Specifies the number of results to return
#'   per page (see cursor below). The default is 20, with a maximum
#'   of 5,000.
#' @param cursor semi-optional Causes the collection of list members
#'   to be broken into "pages" of consistent sizes (specified by
#'   the count parameter). If no cursor is provided, a
#'   value of -1 will be assumed, which is the first "page."
#'   The response from the API will include a previous_cursor
#'   and next_cursor to allow paging back and forth. See Using
#'   cursors to navigate collections for more information.
#' @inheritParams lookup_users
#' @examples
#' if (auth_has_default()) {
#'   subscribers <- lists_subscribers(
#'     slug = "new-york-times-politics",
#'     owner_user = "nytpolitics",
#'   )
#'   subscribers
#' }
#' @family lists
#' @family users
#' @export
lists_subscribers <- function(list_id = NULL,
                              slug = NULL,
                              owner_user = NULL,
                              n = 20,
                              cursor = "-1",
                              parse = TRUE,
                              token = NULL) {

  if (n > 5000) {
    n <- ceiling(n / 5000)
    count <- 5000
  } else {
    count <- n
    n <- 1L
  }
  args <- list(
    list_id = list_id,
    slug = slug,
    owner_user = owner_user,
    n = count,
    cursor = cursor,
    parse = parse,
    token = token
  )
  out <- vector('list', n)
  for (i in seq_along(out)) {
    out[[i]] <- do.call("lists_subscribers_call", args)
    args$cursor <- attr(out[[i]], "next_cursor")
    if (is.null(args$cursor) || identical(args$cursor, "0")) {
      break
    }
  }
  if (parse) {
    out <- do_call_rbind(out)
  }
  out
}


lists_subscribers_call <- function(list_id = NULL,
                                   slug = NULL,
                                   owner_user = NULL,
                                   n = 20,
                                   cursor = "-1",
                                   parse = TRUE,
                                   token = NULL) {
  
  params <- lists_params(
    list_id = list_id,
    slug = slug,
    owner_user = owner_user,
    count = n,
    cursor = cursor
  )
  
  r <- TWIT_get(token, "/1.1/lists/subscribers", params)
  
  if (has_name_(r, "next_cursor_str")) {
    next_cursor <- r[["next_cursor_str"]]
  } else {
    next_cursor <- "0"
  }
  if (parse) {
    r <- as_lists_subscribers(r)
    r <- as.data.frame(r)
  }
  attr(r, "next_cursor") <- next_cursor
  r
}

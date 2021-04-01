#' Get list subscriptions of a given user.
#'
#' @inheritParams get_timeline
#' @param n Specifies the number of results to return
#'   per page (see cursor below). The default is 20, with a maximum
#'   of 1000.
#' @param cursor Causes the collection of list members
#'   to be broken into "pages" of consistent sizes (specified by
#'   the count parameter). If no cursor is provided, a
#'   value of -1 will be assumed, which is the first "page."
#'   The response from the API will include a previous_cursor
#'   and next_cursor to allow paging back and forth. See Using
#'   cursors to navigate collections for more information.
#' @inheritParams lookup_users
#' @examples
#' if (auth_has_default()) {
#'   lists <- lists_subscriptions("kearneymw")
#'   lists
#' }
#' @family lists
#' @export
lists_subscriptions <- function(user,
                                n = 20,
                                cursor = "-1",
                                parse = TRUE,
                                token = NULL) {
  if (n > 1000) {
    n <- ceiling(n / 1000)
    count <- 1000
  } else {
    count <- n
    n <- 1L
  }
  args <- list(
    user = user,
    n = count,
    cursor = cursor,
    parse = parse,
    token = token
  )
  out <- vector('list', n)
  for (i in seq_along(out)) {
    out[[i]] <- do.call("lists_subscriptions_call", args)
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


lists_subscriptions_call <- function(user,
                                     n = 20,
                                     cursor = "-1",
                                     parse = TRUE,
                                     token = NULL) {
  params <- list(
    count = n,
    cursor = cursor
  )
  params[[user_type(user)]] <- user

  r <- TWIT_get(token, "/1.1/lists/subscriptions", params)

  ## get/set next_cursor value
  if (has_name_(r, "next_cursor_str")) {
    next_cursor <- r[["next_cursor_str"]]
  } else {
    next_cursor <- "0"
  }

  ## parse to data frame
  if (parse && has_name_(r, "lists")) {
    r <- as_lists_subscriptions(r$lists)
    r <- as.data.frame(r)
  }

  ## insert cursor
  attr(r, "next_cursor") <- next_cursor

  ## return object
  r
}

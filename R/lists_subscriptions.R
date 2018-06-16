#' Get list subscriptions of a given user.
#'
#' @param user Either the user ID or screen name of user.
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
#' @param parse Logical indicating whether to convert the response object into
#'   an R list. Defaults to TRUE.
#' @param token Every user should have their own Oauth (Twitter API) token. By
#'   default \code{token = NULL} this function looks for the path to a saved
#'   Twitter token via environment variables (which is what `create_token()`
#'   sets up by default during initial token creation). For instruction on how
#'   to create a Twitter token see the tokens vignette, i.e.,
#'   `vignettes("auth", "rtweet")` or see \code{?tokens}.
#' @examples
#'
#' \dontrun{
#'
#' ## get subscriptions of new york times politics list
#' rstats <- lists_subscriptions(
#'   slug = "new-york-times-politics",
#'   n = 1000
#' )
#'
#' }
#'
#' @family lists
#' @export
lists_subscriptions <- function(user,
                                n = 20,
                                cursor = "-1",
                                parse = TRUE,
                                token = NULL) {
  lists_subscriptions_(
    user = user,
    n = n,
    cursor = cursor,
    parse = parse,
    token = token
  )
}

lists_subscriptions_ <- function(user,
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
  ## api path
  query <- "lists/subscriptions"

  ## build params
  params <- list(
    user = user,
    count = n,
    cursor = cursor
  )
  names(params)[1] <- .id_type(user)

  ## validate token
  token <- check_token(token)

  ## build URL
  url <- make_url(query = query, param = params)

  ## request
  r <- httr::GET(url, token)

  ## check status
  warn_for_twitter_status(r)

  ## read content
  r <- from_js(r)

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

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
lists_subscribers <- function(list_id = NULL,
                              slug = NULL,
                              owner_user = NULL,
                              n = 20,
                              cursor = "-1",
                              parse = TRUE,
                              token = NULL) {
  lists_subscribers_(
    list_id = list_id,
    slug = slug,
    owner_user = owner_user,
    n = n,
    cursor = cursor,
    parse = parse,
    token = token
  )
}

lists_subscribers_ <- function(list_id = NULL,
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
  query <- "lists/subscribers"
  if (is.null(list_id) && !is.null(slug) & !is.null(owner_user)) {
    params <- list(
      slug = slug,
      owner_user = owner_user,
      count = n,
      cursor = cursor
    )
    names(params)[2] <- paste0("owner_", .id_type(owner_user))
  } else {
    params <- list(
      list_id = list_id,
      count = n,
      cursor = cursor
    )
  }
  token <- check_token(token)
  url <- make_url(query = query, param = params)
  r <- httr::GET(url, token)
  r <- from_js(r)
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

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
#' @param parse Logical indicating whether to convert the response object into
#'   an R list. Defaults to TRUE.
#' @param token Every user should have their own Oauth (Twitter API) token. By
#'   default \code{token = NULL} this function looks for the path to a saved
#'   Twitter token via environment variables (which is what `create_token()`
#'   sets up by default during initial token creation). For instruction on how
#'   to create a Twitter token see the tokens vignette, i.e.,
#'   `vignettes("auth", "rtweet")` or see \code{?tokens}.
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
  query <- "lists/members"
  stopifnot(is.numeric(n))
  if (n > 5000) {
    warning("maximum number of list users it 5,000")
    n <- 5000
  }
  if (is.null(list_id) && !is.null(slug) & !is.null(owner_user)) {
    params <- list(
      slug = slug,
      owner_user = owner_user,
      count = n,
      cursor = cursor,
      ...
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
  warn_for_twitter_status(r)
  if (r$status_code != 200L) {
    return(data.frame())
  }
  r <- from_js(r)
  if (parse) {
    r <- as_lists_members(r)
    r <- as.data.frame(r)
  }
  r
}

#' Get the members of a specified Twitter list.
#'
#' @param list_id required The numerical id of the list.
#' @param slug required You can identify a list by its slug instead of
#'   its numerical id. If you decide to do so, note that you'll also
#'   have to specify the list owner using the owner_id or
#'   owner_user parameters.
#' @param owner_user optional The screen name or user ID of the user
#'   who owns the list being requested by a slug.
#' @param n Specifies the number of results to return
#'   per page (see cursor below). The default is 20, with a maximum
#'   of 5,000.
#' @param cursor optional Breaks the results into pages. Provide a
#'   value of -1 to begin paging. Provide values as returned in the
#'   response body's next_cursor and previous_cursor attributes to
#'   page back and forth in the list.
#' @param parse Logical indicating whether to convert the response object into
#'   an R list. Defaults to TRUE.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find
#'   instructions on how to create tokens and setup an environment
#'   variable in the tokens vignette (in r, send \code{?tokens} to
#'   console).
#' @param ... Other arguments used as parameters in query composition.
#' @return Either a nested list (if parsed) or an HTTP response object.
#' @examples
#' \dontrun{
#'
#' ## get list memebers for a list of polling experts using list_id
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
  token <- check_token(token, query)
  url <- make_url(query = query, param = params)
  r <- httr::GET(url, token)
  if (parse) {
    r <- from_js(r)
    r <- as_lists_members(r)
    r <- as.data.frame(r)
  }
  r
}

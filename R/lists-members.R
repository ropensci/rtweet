#' GET lists/members
#'
#' Returns the members of the specified list. Private list members
#' will only be shown if the authenticated user owns the specified
#' list.
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
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find
#'   instructions on how to create tokens and setup an environment
#'   variable in the tokens vignette (in r, send \code{?tokens} to
#'   console).
#' @param ... Other args used as parameters in query composition.
#' @param parse Logical indicating whether to convert the response object into
#'   an R list. Defaults to TRUE.
#' @return data
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


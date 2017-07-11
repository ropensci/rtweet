#' GET lists/members¶
#'
#' @param list_id required The numerical id of the list.
#' @param slug required You can identify a list by its slug instead of its numerical id. If you decide
#'   to do so, note that you’ll also have to specify the list owner using the
#'   owner_id or owner_screen_name parameters.
#' @param owner_screen_name optional The screen name of the user who owns the list being requested by a slug.
#' @param owner_id optional The user ID of the user who owns the list being requested by a slug.
#' @param count optional Specifies the number of results to return per page (see cursor below). The
#'   default is 20, with a maximum of 5,000.
#' @param cursor semi-optional Causes the collection of list members to be broken into “pages” of consistent
#'   sizes (specified by the count parameter). If no cursor is provided, a value of
#'   -1 will be assumed, which is the first “page.”
#'   The response from the API will include a previous_cursor and next_cursor
#'   to allow paging back and forth. See Using cursors to navigate
#'   collections for more information.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find
#'   instructions on how to create tokens and setup an environment
#'   variable in the tokens vignette (in r, send \code{?tokens} to
#'   console).
#' @return data
list_members <- function(list_id,
                         owner_user,
                         count = 5000,
                         cursor = "-1",
                         token = NULL) {
  query <- "lists/members"
  params <- list(list_id = list_id,
                 owner_user = owner_user,
                 count = count,
                 cursor = cursor)
  names(params)[2] <- paste0("owner_", .id_type(owner_user))
  token <- check_token(token, query)
  url <- make_url(query = query, param = params)
  r <- httr::GET(url, token)
  from_js(r)
}



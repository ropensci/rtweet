#' lists/memberships
#' 
#' Returns the lists the specified user has been added to. If user_id
#' or screen_name are not provided the memberships for the
#' authenticating user are returned.
#' 
#' @param user The user id or screen_name of the user for whom to
#'   return results for.
#' @param n The amount of results to return per page. Defaults to
#'   20. No more than 1000 results will ever be returned in a single
#'   page.
#' @param cursor optional Breaks the results into pages. Provide a
#'   value of -1 to begin paging. Provide values as returned in the
#'   response body's next_cursor and previous_cursor attributes to
#'   page back and forth in the list. It is recommended to always use
#'   cursors when the method supports them. See [node:10362] for more
#'   information.
#' @param filter_to_owned_lists When set to true . t or 1 , will
#'   return just lists the authenticating user owns, and the user
#'   represented by user_id or screen_name is a member of.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find
#'   instructions on how to create tokens and setup an environment
#'   variable in the tokens vignette (in r, send \code{?tokens} to
#'   console).
#' @param parse Logical indicating whether to convert the response object into
#'   an R list. Defaults to TRUE.
#' @return Either a nested list (if parsed) or an http response object.
#' @export
lists_memberships <- function(user,
                              n = 20,
                              cursor = "-1",
                              filter_to_owned_lists = FALSE,
                              token = NULL,
                              parse = TRUE) {
  lists_memberships_(
    user = user, n = n, cursor = cursor,
    filter_to_owned_lists = filter_to_owned_lists,
    token = token, parse = parse
  )
}

lists_memberships_ <- function(user,
                               n = 20,
                               cursor = "-1",
                               filter_to_owned_lists = FALSE,
                               token = NULL,
                               parse = TRUE) {
  args <- list(
    cursor = cursor,
    filter_to_owned_lists = filter_to_owned_lists,
    token = token,
    parse = parse
  )
  r <- Map("lists_memberships_call", user, n, MoreArgs = args)
  if (parse) {
    r <- do_call_rbind(r)
  }
  r
}


lists_memberships_call <- function(user,
                                   n = 20,
                                   cursor = "-1",
                                   filter_to_owned_lists = FALSE,
                                   token = NULL,
                                   parse = TRUE) {
  query <- "lists/memberships"
  if (filter_to_owned_lists) {
    filter_to_owned_lists <- "t"
  } else {
    filter_to_owned_lists <- NULL
  }
  stopifnot(is.atomic(user), is_n(n))
  if (n > 1000) {
    warning("n is too large. set to max (1000) instead", call. = FALSE)
    n <- 1000
  }
  params <- list(
    user = user,
    count = n,
    cursor = cursor,
    filter_to_owned_lists
  )
  names(params)[1] <- .id_type(user)
  token <- check_token(token, query)
  url <- make_url(query = query, param = params)
  r <- httr::GET(url, token)
  if (parse) {
    r <- from_js(r)
    r <- as_lists_memberships(r)
    r <- as.data.frame(r)
  }
  r
}





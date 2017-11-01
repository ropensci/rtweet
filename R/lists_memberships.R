#' Get the lists a specified user has been added to.
#'
#' @param user The user id or screen_name of the user for whom to
#'   return results for.
#' @inheritParams lists_members
#' @param filter_to_owned_lists When set to true . t or 1 , will
#'   return just lists the authenticating user owns, and the user
#'   represented by user_id or screen_name is a member of.
#' @details Due to deleted or removed lists, the returned number of memberships
#'   is often less than the provided n value. This is a reflection of the API and
#'   not a unique quirk of rtweet.
#' @examples
#' \dontrun{
#'
#' ## get up to 200 list memberships of Nate Silver
#' ns538 <- lists_memberships("NateSilver538", n = 200)
#'
#' ## view data
#' ns538
#'
#' }
#'
#' @rdname lists_members
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

#' Get Twitter list memeberships (lists containing a given user)
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
#' ## get up to 300 Twitter lists that include Nate Silver
#' ns538 <- lists_memberships("NateSilver538", n = 300)
#'
#' ## view data
#' ns538
#'
#' }
#'
#' @rdname lists_members
#' @export
lists_memberships <- function(user = NULL,
                              n = 100,
                              cursor = "-1",
                              filter_to_owned_lists = FALSE,
                              token = NULL,
                              parse = TRUE) {
  stopifnot(is.numeric(n))
  n.times <- n %/% 100
  if (n > 100) {
    n <- 100
  }
  m <- vector("list", n.times)
  for (i in seq_along(m)) {
    m[[i]] <- lists_memberships_(
      user = user, n = n, cursor = cursor,
      filter_to_owned_lists = filter_to_owned_lists,
      token = token, parse = parse
    )
    cursor <- next_cursor(m[[i]])
    if (is.null(cursor) || identical(cursor, "-1")) break
  }
  if (is.list(m) && is.data.frame(m[[1]])) {
    m <- do.call("rbind", m)
  }
  attr(m, "next_cursor") <- cursor
  m
}

lists_memberships_ <- function(user,
                               n = 20,
                               cursor = "-1",
                               filter_to_owned_lists = FALSE,
                               token = NULL,
                               parse = TRUE) {
  if (is.null(user)) {
    user <- ""
  }
  args <- list(
    cursor = cursor,
    filter_to_owned_lists = filter_to_owned_lists,
    token = token,
    parse = parse
  )
  r <- Map("lists_memberships_call", user, n, MoreArgs = args)
  if (parse) {
    r <- do.call("rbind", r)
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
  stopifnot(is.atomic(user), is_n(n))
  if (n > 1000) {
    warning("n is too large. set to max (1000) instead", call. = FALSE)
    n <- 1000
  }
  if (!filter_to_owned_lists && identical(user, "")) {
    stop("argument \"user\" required unless filter_to_owned_lists = TRUE")
  } else if (identical(user, "")) {
    user <- NULL
  }
  params <- list(
    user = user,
    count = n,
    cursor = cursor,
    filter_to_owned_lists = filter_to_owned_lists
  )
  if (!filter_to_owned_lists) {
    names(params)[1] <- .id_type(user)
    params$filter_to_owned_lists <- NULL
  } else {
    params$user <- NULL
  }
  token <- check_token(token)
  url <- make_url(query = query, param = params)
  r <- httr::GET(url, token)
  warn_for_twitter_status(r)
  if (r$status_code != 200L) {
    return(data.frame())
  }
  r <- from_js(r)
  if (is.recursive(r) && "next_cursor_str" %in% names(r)) {
    cursor <- r$next_cursor_str
  } else if (is.recursive(r) && "next_cursor" %in% names(r)) {
    cursor <- r$next_cursor
  } else {
    cursor <- NULL
  }
  if (parse) {
    r <- as_lists_memberships(r)
    r <- as.data.frame(r)
  }
  if (!is.null(r)) {
    attr(r, "next_cursor") <- cursor
  }
  r
}

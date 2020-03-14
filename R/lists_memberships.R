#' Get Twitter list memberships (lists containing a given user)
#'
#' @param user The user id or screen_name of the user for whom to
#'   return results for.
#' @param filter_to_owned_lists When set to true . t or 1 , will
#'   return just lists the authenticating user owns, and the user
#'   represented by user_id or screen_name is a member of.
#' @param previous_cursor If you wish to use previous cursor instead of next,
#'   input value here to override next cursor.
#' @details Due to deleted or removed lists, the returned number of memberships
#'   is often less than the provided n value. This is a reflection of the API and
#'   not a unique quirk of rtweet.
#' @examples
#' \dontrun{
#'
#' ## get up to 1000 Twitter lists that include Nate Silver
#' ns538 <- lists_memberships("NateSilver538", n = 1000)
#'
#' ## view data
#' ns538
#'
#' }
#'
#' @rdname lists_members
#' @export
lists_memberships <- function(user = NULL,
                              n = 200,
                              cursor = "-1",
                              filter_to_owned_lists = FALSE,
                              token = NULL,
                              parse = TRUE,
                              previous_cursor = NULL) {
  stopifnot(is.numeric(n))
  n.times <- ceiling(n / 200)
  if (n > 200) {
    n <- 200
  }
  m <- vector("list", n.times)
  for (i in seq_along(m)) {
    m[[i]] <- lists_memberships_(
      user = user, n = n, cursor = cursor,
      filter_to_owned_lists = filter_to_owned_lists,
      token = token, parse = parse,
      previous_cursor = previous_cursor
    )
    cursor <- next_cursor(m[[i]])
    if (isTRUE("previous_cursor" %in% names(attributes(m[[i]])))) {
      previous_cursor <- attr(m[[i]], "previous_cursor")
    }
    if (is.null(cursor) || identical(cursor, "-1")) break
  }
  if (is.list(m) && is.data.frame(m[[1]])) {
    m <- do.call("rbind", m)
  }
  attr(m, "next_cursor") <- cursor
  attr(m, "previous_cursor") <- previous_cursor
  m
}

lists_memberships_ <- function(user,
                               n = 200,
                               cursor = "-1",
                               filter_to_owned_lists = FALSE,
                               token = NULL,
                               parse = TRUE, previous_cursor = NULL) {
  if (is.null(user)) {
    user <- ""
  }
  args <- list(
    cursor = cursor,
    filter_to_owned_lists = filter_to_owned_lists,
    token = token,
    parse = parse,
    previous_cursor = previous_cursor
  )
  r <- Map("lists_memberships_call", user, n, MoreArgs = args)
  #if (parse) {
  #  r <- do.call("rbind", r)
  #}
  r[[1]]
}


lists_memberships_call <- function(user,
                                   n = 200,
                                   cursor = "-1",
                                   filter_to_owned_lists = FALSE,
                                   token = NULL,
                                   parse = TRUE, previous_cursor = NULL) {
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
    filter_to_owned_lists = filter_to_owned_lists,
    previous_cursor = previous_cursor
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
    previous_cursor <- r$previous_cursor_str
  } else if (is.recursive(r) && "next_cursor" %in% names(r)) {
    cursor <- r$next_cursor
    previous_cursor <- r$previous_cursor
  } else {
    cursor <- NULL
    previous_cursor <- NULL
  }
  if (parse) {
    r <- as_lists_memberships(r)
    r <- as.data.frame(r)
  }
  if (!is.null(r)) {
    attr(r, "next_cursor") <- cursor
    attr(r, "previous_cursor") <- previous_cursor
  }
  r
}

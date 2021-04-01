#' Get user IDs of accounts followed by target user(s).
#'
#' Returns a list of user IDs for the accounts following BY one or
#' more specified users.
#'
#' @inheritParams lookup_users
#' @param users Screen name or user ID of target user from which the
#'   user IDs of friends (accounts followed BY target user) will be
#'   retrieved.
#' @param n Number of friends (user IDs) to return. Defaults to 5,000,
#'   which is the maximum returned by a single API call. Users are
#'   limited to 15 of these requests per 15 minutes. Twitter limits
#'   the number of friends a user can have to 5,000. To follow more
#'   than 5,000 accounts (to have more than 5 thousand "friends")
#'   accounts must meet certain requirements (e.g., a certain ratio of
#'   followers to friends). Consequently, the vast majority of users
#'   follow fewer than five thousand accounts. This function has been
#'   oriented accordingly (i.e., it assumes the maximum value of n is
#'   5000). To return more than 5,000 friends for a single user, call
#'   this function multiple times with requests after the first using
#'   the `page` parameter.
#' @param page Default `page = -1` specifies first page of JSON
#'   results. Other pages specified via cursor values supplied by
#'   Twitter API response object. This is only relevant if a user has
#'   over 5000 friends (follows more than 5000 accounts).
#' @param verbose Logical indicating whether or not to include output
#'   messages. Defaults to TRUE, which includes printing a success message
#'   for each inputted user.
#' @seealso <https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-friends-ids>
#' @examples
#' if (auth_has_default()) {
#'   user <- get_friends("ropensci")
#'   user
#' }
#' @return A tibble data frame with two columns, "user" for name or ID of target
#'   user and "user_id" for follower IDs.
#' @family ids
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/v1/accounts-and-users/follow-search-get-users/api-reference/get-friends-ids>
get_friends <- function(users,
                        n = 5000,
                        retryonratelimit = FALSE,
                        page = "-1",
                        parse = TRUE,
                        verbose = TRUE,
                        token = NULL) {
  
  results <- lapply(users, get_friends_user, 
    n = n, 
    parse = parse,
    token = token
  )
  
  if (parse) {
    results <- do.call("rbind", results)
  }
  
  results
}

get_friends_user <- function(user, token, n = 5000, parse = TRUE) {
  params <- list(stringify_ids = TRUE)
  params[[user_type(user)]] <- user
  
  results <- TWIT_paginate_cursor(token, "/1.1/friends/ids", params, 
    page_size = 5000,
    n = n
  )
  
  if (parse) {
    results <- tibble::tibble(
      user = user,
      ids = unlist(lapply(results, function(x) x$ids))
    )
  }
  results
}


#' Lookup friendship information between users.
#'
#' Gets information on friendship between authenticated user and up
#' to 100 other users.
#'
#' @inheritParams get_timeline
#' @return Data frame converted form returned JSON object. If parse is
#'   not true, the HTTP response object is returned instead.
#' @family friends
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/v1/accounts-and-users/follow-search-get-users/api-reference/get-friendships-lookup>
my_friendships <- function(user,
                           parse = TRUE,
                           token = NULL) {
  params <- list()
  params[[user_type(user)]] <- paste0(user, collapse = ",")
  TWIT_get(token, "/1.1/friendships/lookup", params, parse = parse)
}

#' Lookup friendship information between two specified users.
#'
#' Gets information on friendship between two Twitter users.
#'
#' @inheritParams lookup_users
#' @param source Screen name or user id of source user.
#' @param target Screen name or user id of target user.
#' @return Data frame converted form returned JSON object. If parse is
#'   not true, the HTTP response object is returned instead.
#' @family friends
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/v1/accounts-and-users/follow-search-get-users/api-reference/get-friendships-show>
lookup_friendships <- function(source, target, parse = TRUE, token = NULL) {
  if (length(source) > 1L && length(target) > 1L) {
    stopifnot(length(source) == length(target))
  }

  fds <- Map(
    "lookup_friendships_", source, target,
    MoreArgs = list(parse = parse, token = token)
  )
  if (parse) {
    fds <- do.call("rbind", fds)
    row.names(fds) <- NULL
  }
  fds
}

lookup_friendships_ <- function(source,
                                target,
                                parse = TRUE,
                                token = NULL) {
  stopifnot(is.atomic(source), is.atomic(target))

  params <- list()
  params[[paste0("source_", user_type(source, "source"))]] <- source
  params[[paste0("target_", user_type(target, "target"))]] <- target

  f <- TWIT_get(token, "/1.1/friendships/show", params, parse = parse)
  if (parse) {
    f <- parse_showfriendships(f, source, target)
  }
  f
}

parse_showfriendships <- function(x, source_user, target_user) {
  if (has_name_(x, "relationship")) {
    x <- x$relationship
  }
  if (has_name_(x, "source")) {
    src <- unlist(x$source)
    src <- tibble::tibble(
      relationship = "source",
      user = target_user,
      variable = names(src),
      value = src
    )
  } else {
    src <- data.frame()
  }
  if (has_name_(x, "target")) {
    trg <- unlist(x$target)
    trg <- tibble::tibble(
      relationship = "target",
      user = source_user,
      variable = names(trg),
      value = trg
    )
  } else {
    trg <- data.frame()
  }
  rbind(src, trg)
}

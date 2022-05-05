#' Get user IDs of accounts followed by target user(s).
#'
#' Returns a list of user IDs for the accounts following BY one or
#' more specified users. 
#' 
#' Generally, you should not need to set `n` to more than 5,000 since Twitter
#' limits the number of people that you can follow (i.e. to follow more than
#' 5,000 people at least 5,000 people need to follow you).
#' 
#' @note If a user is protected the API will omit all requests so you'll need 
#' to find which user is protected. rtweet will warn you and the output will be `NA`. 
#'
#' @inheritParams TWIT_paginate_cursor
#' @inheritParams get_followers
#' @param users Screen name or user ID of target user from which the
#'   user IDs of friends (accounts followed BY target user) will be
#'   retrieved.
#' @references <https://developer.twitter.com/en/docs/twitter-api/v1/accounts-and-users/follow-search-get-users/api-reference/get-friends-ids>
#' @examples
#' if (auth_has_default()) {
#' users <- get_friends("ropensci")
#' users
#' }
#' @return A tibble data frame with two columns, "from_id" for name or ID of target
#'   user and "to_id" for accounts ID they follow.
#' @export
get_friends <- function(users,
                        n = 5000,
                        retryonratelimit = NULL,
                        cursor = "-1",
                        parse = TRUE,
                        verbose = TRUE,
                        token = NULL,
                        page = lifecycle::deprecated()) {
  
  if (lifecycle::is_present(page)) {
    lifecycle::deprecate_warn("1.0.0", "get_friends(page)", "get_friends(cursor)")
    cursor <- page
  }

  results <- lapply(users, get_friends_user, 
    n = n, 
    retryonratelimit = retryonratelimit,
    cursor = cursor,
    parse = parse,
    verbose = verbose,
    token = token
  )
  
  if (parse) {
    # Can only paginate with cursor if requesting info for single user. 
    # Fortunately, few people follower >5000 users so this should rarely
    # come up in practice.
    df <- do.call("rbind", results)
    
    if (length(results) == 1) {
      results <- copy_cursor(df, results[[1]])
    } else {
      results <- df
    }
  }
  results
}

get_friends_user <- function(user, token, ..., parse = TRUE) {
  params <- list(stringify_ids = TRUE)
  params[[user_type(user)]] <- user
  
  results <- TWIT_paginate_cursor(token, "/1.1/friends/ids", params,
    page_size = 5000,
    ...
  )

  if (parse) {
    df <- tibble::tibble(
      from_id = user,
      to_id = unlist(lapply(results, function(x) x$ids), 
                     recursive = FALSE, use.names = FALSE)
    )
    if (ncol(df) == 1) {
      df$to_id <- NA
    }
    results <- copy_cursor(df, results)
  }
  results
}


#' Lookup friendship information between users.
#'
#' Gets information on friendship between authenticated user and up
#' to 100 other users.
#'
#' @inheritParams get_timeline
#' @family friends
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/v1/accounts-and-users/follow-search-get-users/api-reference/get-friendships-lookup>
my_friendships <- function(user,
                           parse = FALSE,
                           token = NULL) {
  
  if (!isFALSE(parse)) {
    abort("`my_friendships()` can only return unparsed data")
  }
  
  params <- list()
  params[[user_type(user)]] <- paste0(user, collapse = ",")
  TWIT_get(token, "/1.1/friendships/lookup", params)
}

#' Lookup friendship information between two specified users.
#'
#' Gets information on friendship between two Twitter users.
#'
#' @inheritParams lookup_users
#' @param source Screen name or user id of source user.
#' @param target Screen name or user id of target user.
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

  f <- TWIT_get(token, "/1.1/friendships/show", params)
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

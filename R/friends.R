#' Get user IDs of accounts followed by target user(s).
#'
#' Returns a list of user IDs for the accounts following BY one or
#' more specified users. To return the friends of more than 15 users
#' in a single call (the rate limit maximum), set "retryonratelimit"
#' to TRUE.
#'
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
#'   the \code{page} parameter.
#' @param retryonratelimit If you'd like to retrieve 5,000 or fewer
#'   friends for more than 15 target users, then set
#'   \code{retryonratelimit = TRUE} and this function will use
#'   base \code{Sys.sleep} until rate limits reset and the desired
#'   number of friend networks is retrieved. This defaults to
#'   FALSE. See details for more info regarding possible issues with
#'   timing misfires.
#' @param page Default \code{page = -1} specifies first page of JSON
#'   results. Other pages specified via cursor values supplied by
#'   Twitter API response object. This is only relevant if a user has
#'   over 5000 friends (follows more than 5000 accounts).
#' @param parse Logical, indicating whether to return parsed vector or
#'   nested list object. By default, \code{parse = TRUE}
#'   saves you the time [and frustrations] associated with
#'   disentangling the Twitter API return objects.
#' @param verbose Logical indicating whether or not to include output
#'   messages. Defaults to TRUE, which includes printing a success message
#'   for each inputted user.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find
#'   instructions on how to create tokens and setup an environment
#'   variable in the tokens vignette (in r, send \code{?tokens} to
#'   console).
#' @seealso \url{https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-friends-ids}
#' @examples
#'
#' \dontrun{
#'
#' ## get user ids of accounts followed by Donald Trump
#' (djt <- get_friends("realDonaldTrump"))
#'
#' ## get user ids of accounts followed by (friends) KFC, Trump, and Nate Silver.
#' (fds <- get_friends(c("kfc", "jack", "NateSilver538")))
#'
#' }
#'
#' @details When \code{retryonratelimit = TRUE} this function internally
#'   makes a rate limit API call to get information on (a) the number of requests
#'   remaining and (b) the amount of time until the rate limit resets. So, in
#'   theory, the sleep call should only be called once between waves of data
#'   collection. However, as a fail safe, if a system's time is calibrated such
#'   that it expires before the rate limit reset, or if, in another session, the
#'   user dips into the rate limit, then this function will wait (use Sys.sleep
#'   for a second time) until the next rate limit reset. Users should monitor
#'   and test this before making especially large calls as any systematic issues
#'   could create sizable inefficiencies.
#' @return A tibble data frame with two columns, "user" for name or ID of target
#'   user and "user_id" for follower IDs.
#' @family ids
#' @export
get_friends <- function(users,
                        n = 5000,
                        retryonratelimit = FALSE,
                        page = "-1",
                        parse = TRUE,
                        verbose = TRUE,
                        token = NULL) {
  args <- list(
    users = users,
    n = n,
    retryonratelimit = retryonratelimit,
    page = page,
    parse = parse,
    verbose = verbose,
    token = token
  )
  do.call("get_friends_", args)
}

get_friends_ <- function(users,
                         n = 5000,
                         retryonratelimit = FALSE,
                         page = "-1",
                         parse = TRUE,
                         verbose = TRUE,
                         token = NULL) {
  stopifnot(is.vector(users), is_n(n))
  if (any(is.na(unlist(users)))) {
    warning("Missing users omitted", call. = FALSE)
    users <- na_omit(users)
  }
  if (n < 5000) {
    count <- n
  } else {
    count <- 5000
  }
  ## number of users to return
  n <- length(users)
  ## build URL
  query <- "friends/ids"
  token <- check_token(token, query)
  ## for larger requests implement Sys.sleep
  if (n > 1) {
    ## initialize output object
    f <- vector("list", n)
    ## default (counter) values
    more <- TRUE
    i <- 0L
    ## until friends of n users have been retrieved
    while (more) {
      rl <- rate_limit(token, query)
      n.times <- rl[["remaining"]]
      i <- i + 1L
      params <- list(
        user_type = users[[i]],
        count = count,
        cursor = page,
        stringify_ids = TRUE
      )
      names(params)[1] <- .id_type(users[[i]])
      url <- make_url(
        query = query,
        param = params
      )
      if (retryonratelimit) {
        ## if no calls remaining then sleep until no longer rate limited
        rate_limited <- isTRUE(n.times == 0)
        while (rate_limited) {
          if (verbose) {
            message(
              paste("Waiting about",
                    round(as.numeric(rl$reset, "secs") / 60, 1),
                    "minutes",
                    "for rate limit reset...")
            )
          }
          Sys.sleep(as.numeric(rl$reset, "secs") + 2)
          rl <- rate_limit(token, query)
          n.times <- rl$remaining
          rate_limited <- isTRUE(n.times == 0)
        }
      }
      ## make call
      f[[i]] <- get_friend(url, token = token)
      if (has_name(f[[i]], "errors")) {
        warning(f[[i]]$errors[["message"]], call. = FALSE)
        return(list(data.frame()))
      } else if (parse) {
        nextcursor <- f[["next_cursor"]]
        f[[i]] <- tibble::as_tibble(
          list(user = users[[i]], user_id = f[[i]][["ids"]]),
          validate = FALSE
        )
        attr(f[[i]], "next_cursor") <- nextcursor
      }
      if (verbose) {
        message(paste(i, "friend networks collected!"))
      }
      ## update more (logical)
      more <- isTRUE(i < n)
    }
    ## i don't think this line is needed anymore but just in case
    f <- f[!vapply(f, is.null, logical(1))]
    ## parse into data frame
    if (parse) {
      nextcursors <- lapply(f, attr, "next_cursor")
      f <- do.call("rbind", f)
      attr(f, "next_cursor") <- nextcursors
    }
  } else {
    users <- unlist(users)
    stopifnot(length(users) == 1L)
    ## compose query
    params <- list(
      user_type = users,
      count = count,
      cursor = page,
      stringify_ids = TRUE
    )
    names(params)[1] <- .id_type(users)
    url <- make_url(
      query = query,
      param = params
    )
    ## if !retryonratelimit then if necessary exhaust what can with token
    f <- get_friend(url, token = token)
      if (has_name(f, "errors")) {
        warning(f$errors[["message"]], call. = FALSE)
        return(list(data.frame()))
      } else if (parse) {
        nextcursor <- f[["next_cursor"]]
        f <- tibble::as_tibble(
          list(user = users, user_id = f[["ids"]]),
          validate = FALSE
        )
        attr(f, "next_cursor") <- nextcursor
      }
  }
  f
}

#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
get_friend <- function(url, token = NULL) {
  from_js(httr::GET(url, token))
}


#' @importFrom jsonlite fromJSON
parse.piper.fnd <- function(r) {
  if (isTRUE("content" %in% names(r))) {
    r <- tryCatch(
      jsonlite::fromJSON(rawToChar(r[["content"]])),
      error = function(e) return(NULL))
    r <- fnd_internal(r)
  } else {
    if (length(r) > 1L) {
      r <- lapply(r, fnd_internal)
      r <- do.call("rbind", r)

    } else {
      r <- fnd_internal(r)
    }
  }
  tibble::as_tibble(r, validate = FALSE)
}

fnd_internal <- function(r) {
  if (is.null(r)) return(data.frame(user_id = NA_character_))
  next_cursor <- as.character(r[["next_cursor_str"]])
  usrs <- data.frame(as.character(r[["ids"]]),
                     stringsAsFactors = FALSE)
  names(usrs) <- "user_id"
  if (is.null(next_cursor)) next_cursor <- NA_character_
  attr(usrs, "next_cursor") <- next_cursor
  usrs
}


#' Lookup friendship information between users.
#'
#' Gets information on friendship between authenticated user and up
#' to 100 other users.
#'
#' @param user Screen name or user id of target user.
#' @param parse Logical indicating whether to return parsed data frame.
#'   Defaults to true.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find instructions
#'   on how to create tokens and setup an environment variable in the
#'   tokens vignette (in r, send \code{?tokens} to console).
#' @return Data frame converted form returned JSON object. If parse is
#'   not true, the HTTP response object is returned instead.
#' @family friends
#' @export
my_friendships <- function(user,
                           parse = TRUE,
                           token = NULL) {
  stopifnot(is.atomic(user))
  token <- check_token(token)
  query <- "friendships/lookup"
  params <- list(
    user_type = paste(user, collapse = ",")
  )
  names(params)[1] <- .id_type(user)
  url <- make_url(
    query = query,
    param = params)
  f <- tryCatch(
    TWIT(get = TRUE, url, token),
    error = function(e) return(NULL))
  if (parse) {
    from_js(f)
  } else {
    f
  }
}



lookup_friendships_ <- function(source,
                              target,
                              parse = TRUE,
                              token = NULL) {
  stopifnot(is.atomic(source), is.atomic(target))
  token <- check_token(token)
  query <- "friendships/show"
  params <- list(
    source = source,
    target = target
  )
  names(params)[1] <- paste0("source_", .id_type(source))
  names(params)[2] <- paste0("target_", .id_type(target))
  url <- make_url(
    query = query,
    param = params)
  f <- tryCatch(
    TWIT(get = TRUE, url, token),
    error = function(e) return(NULL))
  f <- from_js(f)
  if (parse) {
    f <- parse_showfriendships(f, source, target)
  }
  f
}

#' Lookup friendship information between two specified users.
#'
#' Gets information on friendship between two Twitter users.
#'
#' @param source Screen name or user id of source user.
#' @param target Screen name or user id of target user.
#' @param parse Logical indicating whether to return parsed data frame.
#'   Defaults to true.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find instructions
#'   on how to create tokens and setup an environment variable in the
#'   tokens vignette (in r, send \code{?tokens} to console).
#' @return Data frame converted form returned JSON object. If parse is
#'   not true, the HTTP response object is returned instead.
#' @family friends
#' @export
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



parse_showfriendships <- function(x, source_user, target_user) {
  if (has_name_(x, "relationship")) {
    x <- x$relationship
  }
  if (has_name_(x, "source")) {
    src <- unlist(x$source)
    src <- tibble::data_frame(
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
    trg <- tibble::data_frame(
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

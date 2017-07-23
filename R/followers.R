#' get_followers
#'
#' @description Returns max followers per token
#' @param user Screen name or user id of target user.
#' @param n Number of followers to return. For max return, enter
#'   \code{n = "all"} or \code{n = 75000} (max per token).
#' @param page Default \code{page = -1} specifies first page of json
#'   results. Other pages specified via cursor values supplied by
#'   Twitter API response object.
#' @param retryonratelimit The max number of followers returned every
#'   15 minutes is 75,000. If you'd like more than that, then you can
#'   set this to TRUE and the function will try to implement a sleep
#'   functional call internally and iterate through until n is achieved.
#'   Defaults to FALSE.
#' @param verbose Logical indicating whether or not to print messages.
#'   Only relevant if retryonratelimit = TRUE. Defaults to TRUE, prints
#'   sleep times and followers gathered counts.
#' @param parse Logical, indicating whether to return parsed
#'   vector or nested list (fromJSON) object. By default,
#'   \code{parse = TRUE} saves you the time [and frustrations]
#'   associated with disentangling the Twitter API return objects.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find instructions
#'   on how to create tokens and setup an environment variable in the
#'   tokens vignette (in r, send \code{?tokens} to console).
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#' @examples
#' \dontrun{
#' # get ids of users following the president of the US
#' pres <- get_followers(user = "potus")
#' pres
#'
#' # get ids of users following the Environmental Protection Agency
#' epa <- get_followers(user = "epa")
#' epa
#' }
#'
#' @return list of follower ids and next page value (presumably
#'   this would be used in loops extracting more than 75,000
#'   followers using either multiple tokens or by waiting out
#'   rate limits)
#' @family ids
#' @export
get_followers.default <- function(user,
                                  n = 5000,
                                  retryonratelimit = FALSE,
                                  page = "-1",
                                  parse = TRUE,
                                  verbose = TRUE,
                                  token = NULL) {
  ## if n == all or Inf then lookup followers count
  if (identical(n, "all") || identical(n, Inf)) {
    usr <- lookup_users(user)
    n <- usr$followers_count
  }
  ## check params
  stopifnot(is_n(n),
            is.atomic(user),
            is.atomic(page),
            isTRUE(length(user) == 1))
  ## build URL
  query <- "followers/ids"
  token <- check_token(token, query)
  params <- list(
    user_type = user,
    count = 5000,
    cursor = page,
    stringify = TRUE
  )
  names(params)[1] <- .id_type(user)
  url <- make_url(
    query = query,
    param = params
  )
  ## for larger requests implement Sys.sleep
  if (retryonratelimit) {
    ## total N
    n5k <- ceiling(n / 5000)
    f <- vector("list", n5k)
    ## default (counter) values
    more <- TRUE
    i <- 0L
    ctr <- 0L
    ## until n followers have been retrieved
    while (more) {
      rl <- rate_limit(token, query)
      n.times <- rl[["remaining"]]
      i <- i + 1L
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
      ## exhaust rate limit
      f[[i]] <- scroller(url, n, n.times, type = "followers", token)
      url$query$cursor <- ncs_(f[[i]])
      ## counter
      ctr <- ctr + n.times * 5000
      if (verbose) {
        message(paste(ctr, "followers!"))
      }
      ## update more (logical)
      more <- more_followers(f[[i]], i, n, ctr)
    }
    ## i don't think this line is needed anymore but just in case
    f <- f[!vapply(f, is.null, logical(1))]
    ## parse into data frame
    if (parse) {
      f <- lapply(f, parse.piper.fs, n = n)
      f <- do.call("rbind", f)
    }
  } else {
    ## if !retryonratelimit then if necessary exhaust what can with token
    rl <- rate_limit(token, query)
    n.times <- rl[["remaining"]]
    f <- scroller(url, n, n.times, type = "followers", token)
    ## drop NULL and parse into data frame
    f <- f[!vapply(f, is.null, logical(1))]
    if (parse) f <- parse.piper.fs(f, n)
  }
  f
}

#' GET followers/ids
#'
#' Get user IDs of accounts following target user.
#'
#' @param user Screen name or user ID of target user from which the user IDs
#'   of followers will be retrieved.
#' @param n Number of followers to return. Defaults to 5000, which is the max
#'   number of followers returned by a single API request. Twitter allows up
#'   to 15 of these requests every 15 minutes, which means 75,000 is the max
#'   number of followers to return without waiting for the rate limit to reset.
#' @param retryonratelimit If you'd like to retrieve more than 75,000 followers
#'   in a single call, then set \code{retryonratelimit = TRUE} and this function
#'   will use \code{\link{Sys.sleep}} until rate limits reset and the desired n
#'   is achieved or the number of total followers is exhausted. This defaults
#'   to FALSE. See details for more info regarding possible issues with timing
#'   misfires.
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
#' @param ... For other possible arguments see \code{\link{get_followers.default}}.
#' @return A tibble data frame of follower IDs (one column named "user_id").
#' @export
get_followers <- function(user, n = 5000, retryonratelimit = FALSE, ...) {
  UseMethod("get_followers")
}

more_followers <- function(f, i, n, ctr) {
  ## if null then return FALSE to prevent error
  if (length(f) == 0L) return(FALSE)
  ## only interested in value of last 'next_cursor'
  f <- f[[length(f)]]
  ## if n > obs, f has nex_cursor, next_cursor != 0
  ##   then yes, TRUE, there are more followers to get
  all(
    n > ctr,
    has_name(f, "next_cursor_str"),
    !isTRUE(identical(`[[`(f, "next_cursor_str"), "0"))
  )
}

ncs_ <- function(f) {
  if (length(f) == 0) return("0")
  f <- f[[length(f)]]
  if (has_name(f, "next_cursor_str")) {
    ## next cursor
    nc <- f[["next_cursor_str"]]
    if (is.null(nc)) return("0")
    return(nc)
  }
  "0"
}



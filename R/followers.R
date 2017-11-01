#' Get user IDs for accounts following target user.
#'
#' Returns a list of user IDs for the accounts following specified
#' user. To return more than 75,000 user IDs in a single call (the
#' rate limit maximum), set "retryonratelimit" to TRUE.
#'
#' @param user Screen name or user ID of target user from which the
#'   user IDs of followers will be retrieved.
#' @param n Number of followers to return. Defaults to 5000, which is
#'   the max number of followers returned by a single API
#'   request. Twitter allows up to 15 of these requests every 15
#'   minutes, which means 75,000 is the max number of followers to
#'   return without waiting for the rate limit to reset.  If this
#'   number exceeds either 75,000 or the remaining number of possible
#'   requests for a given token, then the returned object will only
#'   return what it can (less than n) unless retryonratelimit is set
#'   to true.
#' @param retryonratelimit If you'd like to retrieve more than 75,000
#'   followers in a single call, then set \code{retryonratelimit =
#'   TRUE} and this function will use base \code{Sys.sleep} until rate
#'   limits reset and the desired n is achieved or the number of total
#'   followers is exhausted. This defaults to FALSE. See details for
#'   more info regarding possible issues with timing misfires.
#' @param page Default \code{page = -1} specifies first page of JSON
#'   results. Other pages specified via cursor values supplied by
#'   Twitter API response object. If \code{parse = TRUE} then the
#'   cursor value can be extracted from the return object by using the
#'   \code{next_cursor} function.
#' @param verbose Logical indicating whether or not to print messages.
#'   Only relevant if retryonratelimit = TRUE. Defaults to TRUE,
#'   prints sleep times and followers gathered counts.
#' @param parse Logical, indicating whether to return parsed vector or
#'   nested list object. By default, \code{parse = TRUE}
#'   saves you the time [and frustrations] associated with
#'   disentangling the Twitter API return objects.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find
#'   instructions on how to create tokens and setup an environment
#'   variable in the tokens vignette (in r, send \code{?tokens} to
#'   console).
#' @details When \code{retryonratelimit = TRUE} this function
#'   internally makes a rate limit API call to get information on (a)
#'   the number of requests remaining and (b) the amount of time until
#'   the rate limit resets. So, in theory, the sleep call should only
#'   be called once between waves of data collection. However, as a
#'   fail safe, if a system's time is calibrated such that it expires
#'   before the rate limit reset, or if, in another session, the user
#'   dips into the rate limit, then this function will wait (use
#'   Sys.sleep for a second time) until the next rate limit
#'   reset. Users should monitor and test this before making
#'   especially large calls as any systematic issues could create
#'   sizable inefficiencies.
#' @seealso
#'   \url{https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-followers-ids}
#' @examples
#'
#' \dontrun{
#'
#' ## get 5000 ids of users following the KFC account
#' (kfc <- get_followers("KFC"))
#'
#' ## get max number [per fresh token] of POTUS follower IDs
#' (pres <- get_followers("potus", n = 75000))
#'
#' ## resume data collection (warning: rate limits reset every 15 minutes)
#' pres2 <- get_followers("potus", n = 75000, page = next_cursor(pres))
#'
#' ## store next cursor in object before merging data
#' nextpage <- next_cursor(pres2)
#'
#' ## merge data frames
#' pres <- rbind(pres, pres2)
#'
#' ## store next cursor as an attribute in the merged data frame
#' attr(pres, "next_cursor") <- next_page
#'
#' ## view merged ddata
#' pres
#'
#' }
#'
#' @return A tibble data frame of follower IDs (one column named "user_id").
#' @family ids
#' @export
get_followers <- function(user, n = 5000,
                          page = "-1",
                          retryonratelimit = FALSE,
                          parse = TRUE,
                          verbose = TRUE,
                          token = NULL) {
  args <- list(
    user = user,
    n = n,
    page = page,
    retryonratelimit = retryonratelimit,
    parse = parse,
    verbose = verbose,
    token = token
  )
  do.call("get_followers_", args)
}


get_followers_ <- function(user,
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
  ## if n < 5000, set count to n otherwise seet to 5k
  if (n < 5000) {
    count <- n
  } else {
    count <- 5000
  }
  ## build URL
  query <- "followers/ids"
  token <- check_token(token, query)
  params <- list(
    user_type = user,
    count = count,
    cursor = page,
    stringify_ids = TRUE
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



parse.piper.fs <- function(f, n = NULL) {
  if (!is.list(f)) {
    f <- list(f)
  }
  if (length(f) == 0L) {
    return(data.frame())
  }
  df <- unlist(lapply(f, "[[[", "ids"), use.names = FALSE)
  if (length(df) == 0L) {
    return(data.frame())
  }
  nextcursor <- unlist(lapply(f, "[[[", "next_cursor_str"), use.names = FALSE)
  nextcursor <- na_omit(nextcursor)
  nextcursor <- nextcursor[length(nextcursor)]
  df <- tibble::as_tibble(list(user_id = df), validate = FALSE)
  attr(df, "next_cursor") <- nextcursor
  if (!is.null(n)) {
    if (n < nrow(df)) {
      df <- df[seq_len(n), ]
    }
  }
  df
}

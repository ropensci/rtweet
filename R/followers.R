#' Get user IDs for accounts following target user.
#'
#' Returns a list of user IDs for the accounts following specified
#' user. To return more than 75,000 user IDs in a single call (the
#' rate limit maximum), set "retryonratelimit" to TRUE.
#'
#' @inheritParams lookup_users
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
#'   followers in a single call, then set `retryonratelimit =
#'   TRUE` and this function will use base `Sys.sleep` until rate
#'   limits reset and the desired n is achieved or the number of total
#'   followers is exhausted. This defaults to FALSE. See details for
#'   more info regarding possible issues with timing misfires.
#' @param page Default `page = -1` specifies first page of JSON
#'   results. Other pages specified via cursor values supplied by
#'   Twitter API response object. If `parse = TRUE` then the
#'   cursor value can be extracted from the return object by using the
#'   `next_cursor` function.
#' @param verbose Logical indicating whether or not to print messages.
#'   Only relevant if retryonratelimit = TRUE. Defaults to TRUE,
#'   prints sleep times and followers gathered counts.
#' @details When `retryonratelimit = TRUE` this function
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
#'
#'   At this time, results are ordered with the most recent following first —
#'   however, this ordering is subject to unannounced change and eventual
#'   consistency issues. While this remains true it is possible to iteratively build
#'   follower lists for a user over time.
#' @seealso
#'   <https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-followers-ids>
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

  params <- list(
    count = 5000,
    stringify_ids = TRUE
  )
  params[[.id_type(user)]] <- user

  # TODO: consider if its worth using fastmap::faststack() here
  results <- list()
  i <- 1
  results[[i]] <- TWIT_get(token, "followers/ids", params)
  next_cursor <- results[[i]]$next_cursor_str
  n_seen <- length(results[[i]]$ids)
  
  while (!identical(next_cursor, "0") && n_seen < n) {
    i <- i + 1
    params$cursor <- next_cursor
    results[[i]] <- TWIT_get(token, "followers/ids", params)
    next_cursor <- results[[i]]$next_cursor_str
    n_seen <- n_seen + length(results[[i]]$ids)
  }

  if (parse) {
    results <- lapply(results, parse.piper.fs, n = n)
    results <- do.call("rbind", results)
  }
  
  results
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

  df <- as_tbl(list(user_id = df))
  if (!is.null(n)) {
    if (n < nrow(df)) {
      df <- df[seq_len(n), ]
    }
  }
  df
}

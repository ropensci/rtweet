#' Get one or more user timelines (tweets posted by target user(s)).
#'
#' Returns up to 3,200 statuses posted to the timelines of each of one
#' or more specified Twitter users.
#'
#' @inheritParams lookup_users
#' @param user Vector of user names, user IDs, or a mixture of both.
#' @param n Number of tweets to return per timeline. Defaults to 100.
#'   Must be of length 1 or equal to length of user. This number should
#'   not exceed 3200 as Twitter limits returns to the most recent 3,200
#'   statuses posted or retweeted by each user.
#' @param max_id Character, returns results with an ID less than (that is,
#'   older than) or equal to `max_id`.
#' @param home Logical, indicating whether to return a user-timeline
#'   or home-timeline. By default, home is set to FALSE, which means
#'   `get_timeline` returns tweets posted by the given user. To
#'   return a user's home timeline feed, that is, the tweets posted by
#'   accounts followed by a user, set home to TRUE.
#' @param check Logical indicating whether to remove check available
#'   rate limit. Ensures the request does not exceed the maximum
#'   remaining number of calls.  Defaults to TRUE.
#' @param ... Further arguments passed on as parameters in API query.
#' @return A tbl data frame of tweets data with users data attribute.
#' @seealso
#'   <https://developer.twitter.com/en/docs/tweets/timelines/api-reference/get-statuses-user_timeline>
#' @examples
#'
#' \dontrun{
#'
#' ## get most recent 3200 tweets posted by Donald Trump's account
#' djt <- get_timeline("realDonaldTrump", n = 3200)
#'
#' ## data frame where each observation (row) is a different tweet
#' djt
#'
#' ## users data for realDonaldTrump is also retrieved
#' users_data(djt)
#'
#' ## retrieve timelines of mulitple users
#' tmls <- get_timeline(c("KFC", "ConanOBrien", "NateSilver538"), n = 1000)
#'
#' ## it's returned as one data frame
#' tmls
#'
#' ## count observations for each timeline
#' table(tmls$screen_name)
#'
#' }
#'
#' @family tweets
#' @export
get_timeline <- function(user = NULL,
                         n = 100,
                         max_id = NULL,
                         home = FALSE,
                         parse = TRUE,
                         check = TRUE,
                         token = NULL,
                         ...) {

  stopifnot(is.atomic(user), is.numeric(n))
  user <- user %||% api_screen_name()

  dots <- list(parse = parse, ...)
  rt <- lapply(user, get_timeline_user, 
    n = n, 
    max_id = max_id,
    home = home, 
    parse = parse,
    check = check,
    token = token,
    ...
  )
  
  if (parse) {
    rt <- do.call("rbind", rt)
    rt <- as_tbl(rt)
  } 
  
  rt
}


get_timeline_user <- function(user,
                              n = 200,
                              max_id = NULL,
                              home = FALSE,
                              parse = TRUE,
                              check = TRUE,
                              token = NULL,
                              ...) {
  stopifnot(
    is_n(n),
    is.atomic(user),
    is.atomic(max_id),
    is.logical(home)
  )
  
  api <- if (home) "statuses/home_timeline" else "statuses/user_timeline"

  params <- list(
    max_id = max_id,
    tweet_mode = "extended",
    include_ext_alt_text = "true",
    ...
  )
  params[[.id_type(user)]] <- user

  result <- TWIT_paginate_max_id(token, api, params, 
    get_max_id = function(x) x$id_str,
    n = n,
    page_size = 200,
    parse = parse
  )
  
  if (parse) {
    result <- tweets_with_users(result)
  }
  result
}

#' @export
#' @rdname get_timeline
get_timelines <- function(user,
                          n = 100,
                          max_id = NULL,
                          home = FALSE,
                          parse = TRUE,
                          check = TRUE,
                          token = NULL,
                          ...) {
  get_timeline(user, n, max_id = max_id, home = home, parse = parse, check = check, token = token, ...)
}


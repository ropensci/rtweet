#' Get one or more user timelines
#'
#' `get_timeline()` returns the timeline of any Twitter user (i.e. what they
#' have tweeted). `get_my_timeline()` returns the home timeline for the 
#' authenticated user (i.e. the tweets you see when you log into Twitter).
#'
#' @inheritParams lookup_users
#' @param user Character vector of screen names or user ids. 
#'   See [as_screenname()] for more details.
#' @param n Number of tweets to return per timeline. Defaults to 100.
#'   Must be of length 1 or equal to length of user. This number should
#'   not exceed 3200 as Twitter limits returns to the most recent 3,200
#'   statuses posted or retweeted by each user.
#' @param max_id Character, returns results with an ID less than (that is,
#'   older than) or equal to `max_id`.
#' @param home Logical, indicating whether to return a "user" timeline
#'   (the default, what a user has tweeted/retweeted) or a "home" timeline 
#'   (what the user would see if they logged into twitter). 
#' @param check `r lifecycle::badge("deprecated")`
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

  if (!isFALSE(home)) {
    lifecycle::deprecate_stop("1.0.0", "get_timeline(home)")
  }
  
  user <- user %||% api_screen_name()
  
  rt <- lapply(user, get_timeline_user, 
    n = n, 
    max_id = max_id,
    home = FALSE, 
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


#' @rdname get_timeline
#' @export
get_my_timeline <- function(n = 100,
                            max_id = NULL,
                            parse = TRUE,
                            check = TRUE,
                            token = NULL,
                            ...) {

  get_timeline_user(
    user = api_screen_name(),
    n = n,
    home = TRUE,
    max_id = max_id,
    parse = parse,
    token = token
  )

}

get_timeline_user <- function(user,
                              n = 200,
                              max_id = NULL,
                              home = FALSE,
                              parse = TRUE,
                              token = NULL,
                              ...) {
  stopifnot(
    is.atomic(user),
    is.atomic(max_id),
    is.logical(home)
  )
  
  api <- if (home) "/1.1/statuses/home_timeline" else "/1.1/statuses/user_timeline"

  params <- list(
    max_id = max_id,
    tweet_mode = "extended",
    include_ext_alt_text = "true",
    ...
  )
  params[[user_type(user)]] <- user

  result <- TWIT_paginate_max_id(token, api, params, 
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
#' @usage NULL
get_timelines <- function(user,
                          n = 100,
                          max_id = NULL,
                          home = FALSE,
                          parse = TRUE,
                          check = TRUE,
                          token = NULL,
                          ...) {
  
  lifecycle::deprecate_warn("1.0.0", "get_timelines()", "get_timeline()")
  
  get_timeline(user, n, max_id = max_id, home = home, parse = parse, token = token, ...)
}


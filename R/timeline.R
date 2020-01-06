#' Get one or more user timelines (tweets posted by target user(s)).
#'
#' Returns up to 3,200 statuses posted to the timelines of each of one
#' or more specified Twitter users.
#'
#' @param user Vector of user names, user IDs, or a mixture of both.
#' @param n Number of tweets to return per timeline. Defaults to 100.
#'   Must be of length 1 or equal to length of user. This number should
#'   not exceed 3200 as Twitter limits returns to the most recent 3,200
#'   statuses posted or retweeted by each user.
#' @param max_id Character, returns results with an ID less than (that is,
#'   older than) or equal to `max_id`.
#' @param home Logical, indicating whether to return a user-timeline
#'   or home-timeline. By default, home is set to FALSE, which means
#'   \code{get_timeline} returns tweets posted by the given user. To
#'   return a user's home timeline feed, that is, the tweets posted by
#'   accounts followed by a user, set home to TRUE.
#' @param parse Logical, indicating whether to return parsed
#'   (data.frames) or nested list object. By default, \code{parse =
#'   TRUE} saves users from the time [and frustrations] associated
#'   with disentangling the Twitter API return objects.
#' @param check Logical indicating whether to remove check available
#'   rate limit. Ensures the request does not exceed the maximum
#'   remaining number of calls.  Defaults to TRUE.
#' @param token Every user should have their own Oauth (Twitter API) token. By
#'   default \code{token = NULL} this function looks for the path to a saved
#'   Twitter token via environment variables (which is what `create_token()`
#'   sets up by default during initial token creation). For instructions on how
#'   to create a Twitter token see the tokens vignette, i.e.,
#'   `vignettes("auth", "rtweet")` or see \code{?tokens}.
#' @param ... Further arguments passed on as parameters in API query.
#' @return A tbl data frame of tweets data with users data attribute.
#' @seealso
#'   \url{https://developer.twitter.com/en/docs/tweets/timelines/api-reference/get-statuses-user_timeline}
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
get_timeline <- function(user,
                         n = 100,
                         max_id = NULL,
                         home = FALSE,
                         parse = TRUE,
                         check = TRUE,
                         token = NULL,
                         ...) {
  args <- list(
    user = user,
    n = n,
    home = home,
    max_id = max_id,
    parse = parse,
    check = check,
    token = token,
    ...
  )
  do.call("get_timeline_", args)
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


get_timeline_ <- function(user, n = 100, home = FALSE, ...) {
  ## check inputs
  stopifnot(is.atomic(user), is.numeric(n))
  if (length(user) == 0L) {
    stop("No query found", call. = FALSE)
  }
  ## search for each string in column of queries
  dots <- list(...)

  if (length(dots) > 0L) {
    rt <- Map(get_timeline_call, user = user, n = n, home = home, MoreArgs = dots)
  } else {
    rt <- Map(get_timeline_call, user = user, n = n, home = home)
  }
  if (has_name_(dots, "parse") && isFALSE(dots[["parse"]])) {
    return(rt)
  }
  ## merge tweets data into one data frame
  rt <- do.call("rbind", rt)
  ## return tibble (validate = FALSE makes it a bit faster)
  as_tbl(rt)
}


get_timeline_call <- function(user,
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
  if (home) {
    query <- "statuses/home_timeline"
  } else {
    query <- "statuses/user_timeline"
  }
  if (length(user) > 1) {
    stop("can only return tweets for one user at a time.",
      call. = FALSE)
  }
  token <- check_token(token)
  if (check) {
    rl <- rate_limit(token, query)
    n.times <- rl[["remaining"]]
    if (length(n.times) == 0 || !is.numeric(n.times)) {
      n.times <- 0
    }
    n.times <- n.times[1]
    if (n %/% 200 < n.times) {
      n.times <- ceiling(n / 200L)
    }
  } else {
    rl <- NULL
    n.times <- ceiling(n / 200L)
  }
  if (n.times == 0L) {
    if (!is.null(rl)) {
      reset <- round(as.numeric(rl[["reset"]], "mins"), 2)
    } else {
      reset <- "An unknown number of"
    }
    warning("rate limit exceeded. ",
            round(reset, 2), " mins until rate limit resets.",
            call. = FALSE)
    return(data.frame())
  }
  if (n < 200) {
    count <- n
  } else {
    count <- 200
  }
  params <- list(
    user_type = user,
    count = count,
    max_id = max_id,
    tweet_mode = "extended",
    ...)
  names(params)[1] <- .id_type(user)
  url <- make_url(
    query = query,
    param = params)
  tm <- scroller(url, n, n.times, type = "timeline", token)
  if (parse) {
    tm <- tweets_with_users(tm)
  }
  tm
}


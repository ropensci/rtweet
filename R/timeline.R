#' Get one or more user timelines (tweets posted by target user(s)).
#'
#' Returns up to 3,200 statuses posted to the timelines of each of one
#' or more specified Twitter users.
#'
#' @inheritParams lookup_users
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
#' @param check Logical indicating whether to remove check available
#'   rate limit. Ensures the request does not exceed the maximum
#'   remaining number of calls.  Defaults to TRUE.
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
#' }
#'
#' @family tweets
#' @export
get_timeline <- function(user = NULL,
                         n = 100,
                         max_id = NULL,
                         parse = TRUE,
                         check = TRUE,
                         token = NULL,
                         ...) {
  stopifnot(
    is_n(n),
    is.atomic(user),
    is.atomic(max_id),
    is.logical(check))
 token <- check_token(token)

 if (is.null(user)) {
   query <- "statuses/home_timeline"
 } else {
   query <- "statuses/user_timeline"
 }
 if (length(user) > 1) {
   stop("can only return tweets for one user at a time.",
        call. = FALSE)
 }
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
   include_ext_alt_text = "true",
   ...)
 url <- make_url(
   query = query,
   param = params)
 tm <- scroller(url, n, n.times, type = "timeline", token)
 if (parse) {
   tm <- tweets_with_users(tm)
 }
 tm
}


#' @export
#' @rdname get_timeline
get_timelines <- get_timeline

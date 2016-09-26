#' get_timeline
#'
#' @description Returns timeline of tweets from a specified
#'   Twitter user.
#'
#' @param user Screen name or user id of target user.
#' @param n Numeric, number of tweets to return.
#' @param max_id Character, status_id from which returned tweets
#'   should be older than
#' @param parse Logical, indicating whether to return parsed
#'   (data.frames) or nested list (fromJSON) object. By default,
#'   \code{parse = TRUE} saves users from the time
#'   [and frustrations] associated with disentangling the Twitter
#'   API return objects.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find instructions
#'   on how to create tokens and setup an environment variable in the
#'   tokens vignette (in r, send \code{?tokens} to console).
#' @param \dots Futher arguments passed on to \code{make_url}.
#'   All named arguments that do not match the above arguments
#'   (i.e., count, type, etc.) will be built into the request.
#'   To return only English language tweets, for example, use
#'   \code{lang = "en"}. Or, to exclude retweets, use
#'   \code{include_rts = FALSE}. For more options see Twitter's
#'   API documentation.
#'
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#' @examples
#' \dontrun{
#' # get 2000 from Donald Trump's account
#' djt <- get_timeline("realDonaldTrump", n = 2000)
#'
#' # data frame where each observation (row) is a different tweet
#' djt
#'
#' # users data for realDonaldTrump is also retrieved.
#' # access it via users_data() users_data(hrc)
#' users_data(djt)
#' }
#' @return List consisting of two data frames. One with the tweets
#'   data for a specified user and the second is a single row for
#'   the user provided.
#' @family tweets
#' @export
get_timeline <- function(user, n = 200, max_id = NULL,
  parse = TRUE, token = NULL, ...) {

  query <- "statuses/user_timeline"

  stopifnot(is_n(n), is.atomic(user), is.atomic(max_id))

  if (length(user) > 1) {
    stop("can only return tweets for one user at a time.", call. = FALSE)
  }

  token <- check_token(token, query)

  n.times <- rate_limit(token, query)[["remaining"]]

  params <- list(
    user_type = user,
    count = 200,
    max_id = max_id,
    ...)

  names(params)[1] <- .id_type(user)

  url <- make_url(
    query = query,
    param = params)

  tm <- scroller(url, n, n.times, token)

  if (parse) {
    tm <- parser(tm, n)
    tm <- attr_tweetusers(tm)
  }

  tm
}

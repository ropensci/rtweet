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
#' @param token OAuth token (1.0 or 2.0). By default
#'   \code{token = NULL} fetches a non-exhausted token from
#'   an environment variable.
#' @param \dots Other arguments passed on to \code{make_url}.
#'
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#'
#' @return List consisting of two data frames. One with the tweets
#'   data for a specified user and the second is a single row for
#'   the user provided.
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

  if (parse) tm <- parser(tm, n)

  tm
}

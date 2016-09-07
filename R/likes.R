#' get_likes
#'
#' @description Returns timeline of likes from a specified
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
#' @examples
#' \dontrun{
#' # get 2000 likes from Hadley Wickham's account
#' hadley <- get_likes("hadleywickham", n = 2000, token = twitter_tokens)
#'
#' # data frame where each observation (row) is a different tweet
#' hadley
#'}
#' @return Liked tweets data returned as a tibble data_frame for a specified
#'  user.
#' @export
get_likes <- function(user, n = 200, max_id = NULL,
                         parse = TRUE, token = NULL, ...) {
  
  query <- "favorites/list"
  
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

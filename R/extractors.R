
#' Extracts users data from tweets data object.
#'
#' @param tweets Parsed data object of tweets data as returned via
#'   \code{\link{get_timeline}}, \code{\link{search_tweets}},
#'   \code{\link{stream_tweets}}, etc..
#'
#' @examples
#'
#' \dontrun{
#'
#' ## search for 100 tweets containing the letter r
#' r <- search_tweets("r")
#'
#' ## print tweets data (only first 10 rows are shown)
#' head(r, 10)
#'
#' ## extract users data
#' head(users_data(r))
#'
#' }
#'
#' @return Users data frame from tweets returned in a tweets data object.
#' @aliases user_data data_user data_users
#' @family users
#' @family extractors
#' @export
users_data <- function(tweets) {
  if (!is.recursive(tweets)) return(data.frame())
  if (isTRUE("users" %in% names(attributes(tweets)))) {
    attr(tweets, "users")
  } else {
    data.frame()
  }
}

#' Extracts tweets data from users data object.
#'
#' @param users Parsed data object of users data as returned via
#'   \code{\link{search_users}}, \code{\link{lookup_users}}, etc.
#'
#' @examples
#' \dontrun{
#' ## get twitter user data
#' jack <- lookup_users("jack")
#'
#' ## get data on most recent tweet from user(s)
#' tweets_data(jack)
#'
#' ## search for 100 tweets containing the letter r
#' r <- search_tweets("r")
#'
#' ## print tweets data (only first 10 rows are shown)
#' head(r, 10)
#'
#' ## preview users data
#' head(users_data(r))
#' }
#'
#' @return Tweets data frame.
#'
#' @aliases tweet_data data_tweet data_tweets
#' @family tweets
#' @family extractors
#' @export
tweets_data <- function(users) {
  if (!is.recursive(users)) return(data.frame())
  if (isTRUE("tweets" %in% names(attributes(users)))) {
    attr(users, "tweets")
  } else {
    data.frame()
  }
}

#' next_cursor
#'
#' @description Returns next cursor value from ids object. Return
#'   object used to retrieve next page of results from API request.
#'
#' @param ids Data frame of Twitter IDs generated via
#'   \code{\link{get_followers}} or \code{\link{get_friends}}.
#'
#' @examples
#' \dontrun{
#' # Retrieve user ids of accounts following POTUS
#' f1 <- get_followers("potus", n = 75000)
#' page <- next_cursor(f1)
#'
#' # max. number of ids returned by one token is 75,000 every 15
#' # minutes, so you'll need to wait a bit before collecting the
#' # next batch of ids
#' sys.Sleep(15*60) # Suspend execution of R expressions for 15 mins
#'
#' # Use the page value returned from \code{next_cursor} to continue
#' # where you left off.
#' f2 <- get_followers("potus", n = 75000, page = page)
#' }
#'
#' @return Character string of next cursor value used to retrieved
#'   the next page of results. This should be used to resume data
#'   collection efforts that were interrupted by API rate limits.
#'   Modify previous data request function by entering the returned
#'   value from \code{next_cursor} for the \code{page} argument.
#' @aliases user_data data_user data_users
#' @family users
#' @export
next_cursor <- function(ids) {
	attr(ids, "next_cursor")
}

#' users_data
#'
#' @description Returns users data table (tibble) from tweets data
#'   object.
#'
#' @param tweets Data frame of Twitter statuses (tweets) generated via
#'   \code{\link{get_timeline}}, \code{\link{search_tweets}}, or
#'   \code{\link{stream_tweets}}.
#'
#' @examples
#' \dontrun{
#' # search for 100 tweets containing the letter r
#' r <- search_tweets("r")
#'
#' # print tweets data (only first 10 rows are shown)
#' r
#'
#' # extract users data
#' users_data(r)
#' }
#'
#' @return Tibble data frame of Twitter statuses (tweets) from users
#'   found in a tweets data object.
#' @aliases user_data data_user data_users
#' @family users
#' @export
users_data <- function(tweets) {
	stopifnot(is.data.frame(tweets))
	if (!"users" %in% names(attributes(tweets))) {
		return(tweets)
	} else {
		return(attr(tweets, "users"))
	}
}

#' tweets_data
#'
#' @description Returns tweets data table (tibble) from users data
#'   object.
#'
#' @param users Data frame of Twitter users generated via
#'   \code{lookup_users} or \code{search_users}.
#'
#' @examples
#' \dontrun{
#' # search for 100 tweets containing the letter r
#' r <- search_tweets("r")
#'
#' # print tweets data (only first 10 rows are shown)
#' r
#'
#' # extract users data
#' users_data(r)
#' }
#'
#' @return Tibble data frame of most recent tweets (if available)
#'   of accounts found in users data object.
#'
#' @aliases tweet_data data_tweet data_tweets
#' @export
tweets_data <- function(users) {
	stopifnot(is.data.frame(users))
	if (!"tweets" %in% names(attributes(users))) {
		return(users)
	} else {
		return(attr(users, "tweets"))
	}
}



attr_tweetusers <- function(x) {
	stopifnot(is.list(x), isTRUE(length(x) == 2))
	d <- data.frame()
	if (identical(names(x)[1], "tweets")) {
		d <- x[["tweets"]]
		attr(d, "users") <- x[["users"]]
	}
	if (identical(names(x)[1], "users")) {
		d <- x[["users"]]
		attr(d, "tweets") <- x[["tweets"]]
	}
	d
}
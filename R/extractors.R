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
#' @aliases next_page cursor_next
#' @family ids
#' @export
next_cursor <- function(ids) {
	attr(ids, "next_cursor")
}

#' users_data
#'
#' @description Returns users data frame from returned tweets
#'   data object.
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
#' @return Users data frame from tweets returned in a tweets data object.
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
#' @description Tweets data frame from users returned in a users data object.
#'   Typically, this involves the most recent tweet of each user, though
#'   in some cases the most recent tweet may not be available.
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
#' @return Tweets data frame.
#'
#' @aliases tweet_data data_tweet data_tweets
#' @family tweets
#' @export
tweets_data <- function(users) {
	stopifnot(is.data.frame(users))
	if (!"tweets" %in% names(attributes(users))) {
		return(users)
	} else {
		return(attr(users, "tweets"))
	}
}

#' meta_data
#'
#' @description Returns meta data search information.
#'
#' @param x Data frame of class rtweet_table.
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
#'
#' # extract meta datadata
#' meta_data(r)
#' }
#'
#' @return List of parameters used in \code{search_users}.
#'
#' @export
meta_data <- function(x) {
	attr(x, "meta_search")
}

attr_tweetusers <- function(x) {

	stopifnot(is.list(x))

	if (identical(names(x)[1], "tweets")) {
		d <- x[["tweets"]]
		attr(d, "users") <- x[["users"]]
		attr(d, "meta_search") <- x[["meta_search"]]
	}
	if (identical(names(x)[1], "users")) {
		d <- x[["users"]]
		attr(d, "tweets") <- x[["tweets"]]
		attr(d, "meta_search") <- x[["meta_search"]]
	}
	d
}

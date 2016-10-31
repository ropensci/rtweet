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
	if (!"users" %xy% attributes(tweets)) {
		return(tweets)
	} else {
		return(attr(tweets, "users"))
	}
}

all_tw <- function(search = TRUE) {
	if (search) {
		x <- " OR "
	} else {
		x <- ","
	}
	paste(letters, collapse = x)
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


attr_tweetusers <- function(x) {

	stopifnot(is.list(x))

	if (identical(names(x)[1], "tweets")) {
		if (is.null(x[["tweets"]])) {
			d <- data.frame()
		} else {
			d <- x[["tweets"]]
		}
		if ("users" %in% names(x)) {
			if (is.null(x[["users"]])) {
			attr(d, "users") <- data.frame()
		} else {
			attr(d, "users") <- x[["users"]]
		}
		}
	}
	if (identical(names(x)[1], "users")) {
		if (is.null(x[["users"]])) {
			d <- data.frame()
		} else {
			d <- x[["users"]]
		}
		if ("tweets" %in% names(x)) {
			if (is.null(x[["tweets"]])) {
			attr(d, "tweets") <- data.frame()
		} else {
			attr(d, "tweets") <- x[["tweets"]]
		}
		}
	}
	d
}

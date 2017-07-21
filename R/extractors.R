#' next_cursor
#'
#' Returns next cursor value from ids object. Return
#'   object used to retrieve next page of results from API request.
#'
#' @param ids Data frame of Twitter IDs generated via
#'   \code{\link{get_followers}} or \code{\link{get_friends}}.
#'
#' @examples
#' \dontrun{
#' ## Retrieve user ids of accounts following POTUS
#' f1 <- get_followers("potus", n = 75000)
#'
#' ## store next_cursor in page
#' page <- next_cursor(f1)
#'
#' ## max. number of ids returned by one token is 75,000 every 15
#' ##   minutes, so you'll need to wait a bit before collecting the
#' ##   next batch of ids
#' sys.Sleep(15 * 60) ## Suspend execution of R expressions for 15 mins
#'
#' ## Use the page value returned from \code{next_cursor} to continue
#' ##   where you left off.
#' f2 <- get_followers("potus", n = 75000, page = page)
#'
#' ## combine
#' f <- do.call("rbind", list(f1, f2))
#'
#' ## count rows
#' nrow(f)
#' }
#'
#' @return Character string of next cursor value used to retrieved
#'   the next page of results. This should be used to resume data
#'   collection efforts that were interrupted by API rate limits.
#'   Modify previous data request function by entering the returned
#'   value from \code{next_cursor} for the \code{page} argument.
#' @aliases next_page cursor_next
#' family ids
#' @export
next_cursor <- function(ids) {
    x <- attr(ids, "next_cursor")
    if (is.numeric(x)) {
        op <- options()
        on.exit(options(op))
        options(scipen = 10)
        x <- as.character(x)
    }
    x
}

#' users_data
#'
#' Returns users data frame from returned tweets
#'   data object.
#'
#' @param tweets Data frame of Twitter statuses (tweets) generated via
#'   \code{\link{get_timeline}}, \code{\link{search_tweets}}, or
#'   \code{\link{stream_tweets}}.
#'
#' @examples
#' \dontrun{
#' ## search for 100 tweets containing the letter r
#' r <- search_tweets("r")
#'
#' ## print tweets data (only first 10 rows are shown)
#' head(r, 10)
#'
#' ## extract users data
#' head(users_data(r))
#' }
#'
#' @return Users data frame from tweets returned in a tweets data object.
#' @aliases user_data data_user data_users
#' @family users
#' @export
users_data <- function(tweets) {
  if(!is.recursive(tweets)) return(data.frame())
  if (isTRUE("users" %in% names(attributes(tweets)))) {
    attr(tweets, "users")
  } else {
    tweets
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
firehose <- function(search = TRUE) {
  all_tw(search)
}


#' tweets_data
#'
#' Tweets data frame from users returned in a users data object.
#'   Typically, this involves the most recent tweet of each user, though
#'   in some cases the most recent tweet may not be available.
#'
#' @param users Data frame of status data generated via
#'   \code{search_tweets}, \code{stream_tweets}, \code{get_timeline},
#'   \code{lookup_statuses}, etc.
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
#' @export
tweets_data <- function(users) {
  if(!is.recursive(users)) return(data.frame())
  if (isTRUE("tweets" %in% names(attributes(users)))) {
    attr(users, "tweets")
  } else {
    users
  }
}


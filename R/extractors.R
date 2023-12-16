#' Get tweets from users, or users from tweets
#'
#' Twitter API endpoints that return tweets also return data about the users who
#' tweeted, and most endpoints that return users also return their last tweet.
#' Showing these additional columns would clutter the default display, so
#' rtweet instead stores in special attributes and allows you to show them
#' with the `user_data()` and `tweets_data()` helpers.
#'
#' @return `user_data()` returns a data frame of users; `tweets_data()`
#'   returns a data frame of tweets.
#' @param tweets A data frame of tweets.
#' @export
users_data <- function(tweets) {
  users <- attr(tweets, "users", exact = TRUE)
  if (is.null(users)) {
    abort("`tweets` does not have a `users` attribute", call = current_call())
  }
  users
}

#' @param users A data frame of users.
#' @rdname users_data
#' @export
tweets_data <- function(users) {
  tweets <- attr(users, "tweets", exact = TRUE)
  if (is.null(tweets)) {
    abort("`users` does not have a `tweets` attribute", call = current_call())
  }
  tweets
}

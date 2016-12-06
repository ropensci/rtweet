#' post_tweet
#'
#' @description Posts status update to user's Twitter account
#'
#' @param status Character, tweet status. Must be 140
#'   characters or less.
#' @param token OAuth token. By default \code{token = NULL}
#'   fetches a non-exhausted token from an environment
#'   variable tokens.
#'
#' @examples
#' \dontrun{
#' post_tweet("my first rtweet #rstats")
#' }
#' @export
post_tweet <- function(status = "my first rtweet #rstats",
                       token = NULL) {

	query <- "statuses/update"
	stopifnot(is.character(status))
	if (nchar(status) > 140) {
		stop("cannot exceed 140 characters.", call. = FALSE)
	}
	if (length(status) > 1) {
		stop("can only post one status at a time",
			call. = FALSE)
	}
	token <- check_token(token, query)

	params <- list(status = status)

	url <- make_url(query = query, param = params)

	r <- TWIT(get = FALSE, url, token)

	if (r$status_code != 200) message("something didn't work")

	message("your tweet has been posted!")
}






#' follow_user
#'
#' @description Follows target twitter user.
#'
#' @param user Screen name or user id of target user.
#' @param notify Logical indicating whether to enable notifications
#'   for target user. Defaults to false.
#' @param token OAuth token. By default \code{token = NULL}
#'   fetches a non-exhausted token from an environment
#'   variable tokens.
#'
#' @examples
#' \dontrun{
#' follow_user("BarackObama")
#' }
#' @export
follow_user <- function(user, notify = FALSE, token = NULL) {

  stopifnot(is.atomic(user), is.logical(notify))

  token <- check_token(token)

  query <- "friendships/create"

  params <- list(
    user_type = user,
    notify = notify)

  names(params)[1] <- .id_type(user)

  url <- make_url(query = query, param = params)

  r <- TWIT(get = FALSE, url, token)

  if (r$status_code != 200) message("something didn't work")

  r
}


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
#' @param destroy Logical indicating whether to post (add) or
#'   remove (delete) target tweet as favorite.
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
follow_user <- function(user,
                        destroy = FALSE,
                        notify = FALSE,
                        token = NULL) {

    stopifnot(is.atomic(user), is.logical(notify))

    token <- check_token(token)

    if (destroy) {
        query <- "friendships/destroy"
    } else {
        query <- "friendships/create"
    }

    params <- list(
        user_type = user,
        notify = notify)

    names(params)[1] <- .id_type(user)

    url <- make_url(query = query, param = params)

    r <- TWIT(get = FALSE, url, token)

    if (r$status_code != 200) message("something didn't work")

    r
}

#' unfollow_user
#'
#' Remove, or unfollow, current twitter friend. Wrapper function
#'   for destroy version of follow_user.
#'
#' @param user Screen name or user id of target user.
#' @param token OAuth token. By default \code{token = NULL}
#'   fetches a non-exhausted token from an environment
#'   variable tokens.
#' @export
unfollow_user <- function(user, token = NULL) {
    follow_user(user, destroy = TRUE, token = token)
}

#' favorite_tweet
#'
#' @description Favorites target status id.
#'
#' @param status_id Status id of target tweet.
#' @param destroy Logical indicating whether to post (add) or
#'   remove (delete) target tweet as favorite.
#' @param include_entities Logical indicating whether to
#'   include entities object in return.
#' @param token OAuth token. By default \code{token = NULL}
#'   fetches a non-exhausted token from an environment
#'   variable tokens.
#'
#' @examples
#' \dontrun{
#' rt <- search_tweets("rstats")
#' for (i in rt$user_id) {
#'     favorite_tweet(i)
#' }
#' }
#' @export
favorite_tweet <- function(status_id,
                           destroy = FALSE,
                           include_entities = FALSE,
                           token = NULL) {

    stopifnot(is.atomic(status_id))

    token <- check_token(token)

    if (destroy) {
        query <- "favorites/destroy"
    } else {
        query <- "favorites/create"
    }

    params <- list(
        id = status_id)

    url <- make_url(query = query, param = params)

    r <- TWIT(get = FALSE, url, token)

    if (r$status_code != 200) message("something didn't work")

    invisible(r)
}


#' friendship_update
#'
#' Updates friendship notifications and retweet abilities.
#'
#' @param user Screen name or user id of target user.
#' @param device Logical indicating whether to enable or disable
#'    device notifications from target user behaviors. Defaults
#'    to false.
#' @param retweets Logical indicating whether to enable or disable
#'    retweets from target user behaviors. Defaults to false.
#' @param token OAuth token. By default \code{token = NULL}
#'   fetches a non-exhausted token from an environment
#'   variable tokens.
#'
#' @export
friendship_update <- function(user,
                              device = FALSE,
                              retweets = FALSE,
                              token = NULL) {

    stopifnot(is.atomic(user), is.logical(device),
              is.logical(retweets))

    token <- check_token(token)

    query <- "friendships/update"

    params <- list(
        user_type = user,
        device = device,
        retweets = retweets)

    names(params)[1] <- .id_type(user)

    url <- make_url(query = query, param = params)

    r <- TWIT(get = FALSE, url, token)

    if (r$status_code != 200) message("something didn't work")

    invisible(r)
}


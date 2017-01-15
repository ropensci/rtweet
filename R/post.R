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
#' @family post
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

    if (r$status_code != 200) {
        message(paste0(
            "something didn't work. are you using a token associated ",
            "with *your* Twitter account? if so you may need to set read/write ",
            "permissions or reset your token at apps.twitter.com."))
    }

    message("your tweet has been posted!")
}


#' post_follow
#'
#' @description Follows target twitter user.
#'
#' @param user Screen name or user id of target user.
#' @param destroy Logical indicating whether to post (add) or
#'   remove (delete) target tweet as favorite.
#' @param mute Logical indicating whether to mute the intended
#'   friend (you must already be following this account prior
#'   to muting them)
#' @param notify Logical indicating whether to enable notifications
#'   for target user. Defaults to false.
#' @param retweets Logical indicating whether to enable retweets
#'   for target user. Defaults to true.
#' @param token OAuth token. By default \code{token = NULL}
#'   fetches a non-exhausted token from an environment
#'   variable tokens.
#' @aliases follow_user
#' @examples
#' \dontrun{
#' post_follow("BarackObama")
#' }
#' @family post
#' @export
post_follow <- function(user,
                        destroy = FALSE,
                        mute = FALSE,
                        notify = FALSE,
                        retweets = TRUE,
                        token = NULL) {

    stopifnot(is.atomic(user), is.logical(notify))

    token <- check_token(token)

    if (all(!destroy, !retweets)) {
        query <- "friendships/update"
        params <- list(
            user_type = user,
            notify = notify,
            retweets = retweets)
    } else if (mute) {
        query <- "mutes/users/create"
        params <- list(
            user_type = user)
    } else if (destroy) {
        query <- "friendships/destroy"
        params <- list(
            user_type = user,
            notify = notify)
    } else {
        query <- "friendships/create"
        params <- list(
            user_type = user,
            notify = notify)
    }

    names(params)[1] <- .id_type(user)

    url <- make_url(query = query, param = params)

    r <- TWIT(get = FALSE, url, token)

    if (r$status_code != 200) {
        message(paste0(
            "something didn't work. are you using a token associated ",
            "with *your* Twitter account? if so you may need to set read/write ",
            "permissions or reset your token at apps.twitter.com."))
    }

    r
}

#' post_unfollow
#'
#' Remove, or unfollow, current twitter friend. Wrapper function
#'   for destroy version of follow_user.
#'
#' @param user Screen name or user id of target user.
#' @param token OAuth token. By default \code{token = NULL}
#'   fetches a non-exhausted token from an environment
#'   variable tokens.
#' @aliases unfollow_user
#' @family post
#' @export
post_unfollow_user <- function(user, token = NULL) {
    post_follow(user, destroy = TRUE, token = token)
}

#' post_mute
#'
#' Mute, or hide all content coming from, current twitter friend.
#'   Wrapper function for mute version of follow_user.
#'
#' @param user Screen name or user id of target user.
#' @param token OAuth token. By default \code{token = NULL}
#'   fetches a non-exhausted token from an environment
#'   variable tokens.
#' @aliases mute_user
#' @family post
#' @export
post_mute <- function(user, token = NULL) {
    post_follow(user, mute = TRUE, token = token)
}


#' post_favorite
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
#' @aliases post_favourite favorite_tweet
#' @examples
#' \dontrun{
#' rt <- search_tweets("rstats")
#' r <- lapply(rt$user_id, post_favorite)
#' }
#' @family post
#' @export
post_favorite <- function(status_id,
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

    if (r$status_code != 200) {
        message(paste0(
            "something didn't work. are you using a token associated ",
            "with *your* Twitter account? if so you may need to set read/write ",
            "permissions or reset your token at apps.twitter.com."))
    }
    invisible(r)
}


#' post_friendship
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
#' @aliases friendship_update
#' @family post
#' @export
post_friendship <- function(user,
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

    if (r$status_code != 200) {
        message(paste0(
            "something didn't work. are you using a token associated ",
            "with *your* Twitter account? if so you may need to set read/write ",
            "permissions or reset your token at apps.twitter.com."))
    }
    invisible(r)
}


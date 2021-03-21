#' Follows target twitter user.
#'
#' @inheritParams lookup_users
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

  if (all(!destroy, !retweets)) {
    query <- "/1.1/friendships/update"
    params <- list(
      notify = notify,
      retweets = retweets)
  } else if (mute) {
    query <- "/1.1/mutes/users/create"
    params <- list(r)
  } else if (destroy) {
    query <- "/1.1/friendships/destroy"
    params <- list(
      notify = notify)
  } else {
    query <- "/1.1/friendships/create"
    params <- list(
      notify = notify
    )
  }
  params[[user_type(user)]] <- user

  TWIT_post(token, query, params)
}



#' @aliases unfollow_user
#' @rdname post_follow
#' @export
post_unfollow_user <- function(user, token = NULL) {
  post_follow(user, destroy = TRUE, token = token)
}

#' @aliases unfollow_user
#' @rdname post_follow
#' @aliases mute_user
#' @export
post_mute <- function(user, token = NULL) {
  post_follow(user, mute = TRUE, token = token)
}

#' Updates friendship notifications and retweet abilities.
#'
#' @inheritParams lookup_users
#' @param user Screen name or user id of target user.
#' @param device Logical indicating whether to enable or disable
#'    device notifications from target user behaviors. Defaults
#'    to false.
#' @param retweets Logical indicating whether to enable or disable
#'    retweets from target user behaviors. Defaults to false.
#' @aliases friendship_update
#' @family post
#' @export
post_friendship <- function(user,
                            device = FALSE,
                            retweets = FALSE,
                            token = NULL) {

  stopifnot(is.atomic(user), is.logical(device),
            is.logical(retweets))

  params <- list(
    device = device,
    retweets = retweets
  )
  params[[user_type(user)]] <- user

  TWIT_post(token, "/1.1/friendships/update", params)
}

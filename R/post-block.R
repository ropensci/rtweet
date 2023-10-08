#' Blocking or unblocking twitter users
#'
#' `user_block(...)` blocks or unblocks a target twitter user.
#' `user_unblock(...)` is synonymous to `user_block(..., unblock=TRUE)`.
#'
#' @inheritParams get_timeline
#' @param unblock Logical indicating whether to unblock the intended
#'   friend.
#' @aliases user_unblock
#' @export
#' @references
#' Block: <https://developer.twitter.com/en/docs/twitter-api/v1/accounts-and-users/mute-block-report-users/api-reference/post-blocks-create>
#' @examples
#' if (auth_has_default()) {
#'   user_block("rtweet")
#'   user_unblock("rtweet")
#'   user_block("rtweet", unblock=TRUE) #<-same as the above
#' }
user_block <- function(user,
                      unblock = FALSE,
                      token = NULL) {

  stopifnot(is.atomic(user) || is.null(user), is_logical(unblock))

  if (!unblock) {
    query <- "/1.1/blocks/create"
    params <- list()
  } else {
    query <- "/1.1/blocks/destroy"
    params <- list()
  }
  params[[user_type(user)]] <- user

  TWIT_post(token, query, params)
}


#' @rdname user_block
#' @export
user_unblock <- function(user, token = NULL) {
  user_block(user, unblock = TRUE, token = token)
}


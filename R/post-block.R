#' Blocking or unblocking twitter users
#' 
#' `block_user(...)` blocks or unblocks a target twitter user.
#' `unblock_user(...)` is synonymous to `block_user(..., unblock=TRUE)`
#'
#' @inheritParams get_timeline
#' @param unblock Logical indicating whether to unblock the intended
#'   friend 
#' @aliases unblock_user
#' @examples
#' \dontrun{
#' block_user("BarackObama")
#' unblock_user("BarackObama")
#' block_user("BarackObama", unblock=TRUE) #<-same as the above
#' }
#' @family post
#' @export
#' @references 
#' Block: <https://developer.twitter.com/en/docs/twitter-api/v1/accounts-and-users/mute-block-report-users/api-reference/post-blocks-create>

block_user <- function(user,
                      unblock = FALSE,
                      token = NULL) {

  stopifnot(is.atomic(user), is.logical(unblock))

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


#' @rdname post_block
#' @export
unblock_user <- function(user, token = NULL) {
  block_user(user, unblock = TRUE, token = token)
}


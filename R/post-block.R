#' Follows target twitter user.
#'
#' @inheritParams get_timeline
#' @param block Logical indicating whether to block the intended
#'   friend 
#' @param unblock Logical indicating whether to unblock the intended
#'   friend 
#' @aliases post_unblock
#' @examples
#' \dontrun{
#' post_block("BarackObama")
#' post_unblock("BarackObama")
#' post_block("BarackObama")
#' post_block("BarackObama", unblock=TRUE)
#' }
#' @family post
#' @export
#' @references 
#' Block: <https://developer.twitter.com/en/docs/twitter-api/v1/accounts-and-users/mute-block-report-users/api-reference/post-blocks-create>

post_block <- function(user,
                      unblock = FALSE,
                      token = NULL) {

  stopifnot(is.atomic(user), is.logical(notify))

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
post_unblock <- function(user, token = NULL) {
  post_block(user, unblock = TRUE, token = token)
}


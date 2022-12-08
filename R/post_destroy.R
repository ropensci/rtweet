#' Delete status of user's Twitter account
#'
#' Deletes a status of user's profile.
#' @inheritParams post_tweet
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/v1/tweets/post-and-engage/api-reference/post-statuses-destroy-id>
#' @examples
#' if (auth_has_default()) {
#'   pt <- post_tweet("Running #rtweet examples")
#'   post_destroy(ids(pt))
#' }
post_destroy <- function(destroy_id, token = NULL) {
  stopifnot(is.character(destroy_id) && length(destroy_id) == 1)

  query <- sprintf("/1.1/statuses/destroy/%s", destroy_id)
  r <- TWIT_post(token, query)
  class(r) <- c("tweet_deleted", class(r))
  message("Your tweet has been deleted!")
  return(invisible(r))
}

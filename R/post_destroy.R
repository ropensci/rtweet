#' Delete status of user's Twitter account
#'
#' Deletes a status of user's profile.
#' @inheritParams post_tweet
#' @export
#' @examples
#' \dontrun{
#' pt <- post_tweet()
#' crt <- httr::content(pt)
#' post_destroy(crt$id_str)
#' }
post_destroy <- function(destroy_id, token = NULL) {
  stopifnot(is.character(destroy_id) && length(destroy_id) == 1)
  
  query <- sprintf("1.1/statuses/destroy/%s", destroy_id)
  r <- TWIT_post(token, query)
  message("your tweet has been deleted!")
  return(invisible(r))
}

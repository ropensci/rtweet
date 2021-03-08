#' Favorites target status id.
#'
#' @inheritParams lookup_users
#' @param status_id Status id of target tweet.
#' @param destroy Logical indicating whether to post (add) or
#'   remove (delete) target tweet as favorite.
#' @param include_entities Logical indicating whether to
#'   include entities object in return.
#' @aliases post_favourite favorite_tweet
#' @examples
#' \dontrun{
#' rt <- search_tweets("#rstats", n = 1)
#' post_favorite(rt$status_id)
#' }
#' @family post
#' @export
post_favorite <- function(status_id,
                          destroy = FALSE,
                          include_entities = FALSE,
                          token = NULL) {

  stopifnot(is.atomic(status_id))

  if (destroy) {
    query <- "/1.1/favorites/destroy"
  } else {
    query <- "/1.1/favorites/create"
  }

  params <- list(id = status_id)
  TWIT_post(token, query, params)
}


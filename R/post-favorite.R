#' Favorites target status id.
#'
#' `r lifecycle::badge("deprecated")`
#' @inheritParams lookup_users
#' @param status_id Status id of target tweet.
#' @param destroy Logical indicating whether to post (add) or
#'   remove (delete) target tweet as favorite.
#' @param include_entities Logical indicating whether to
#'   include entities object in return.
#' @aliases post_favourite favorite_tweet
#' @family post
#' @seealso [`rtweet-deprecated`]
#' @export
#' @references
#' Create: <https://developer.twitter.com/en/docs/twitter-api/v1/tweets/post-and-engage/api-reference/post-favorites-create>
#' Destroy: <https://developer.twitter.com/en/docs/twitter-api/v1/tweets/post-and-engage/api-reference/post-favorites-destroy>
post_favorite <- function(status_id,
                          destroy = FALSE,
                          include_entities = FALSE,
                          token = NULL) {

  stopifnot(is.atomic(status_id) && !is.null(status_id))

  if (destroy) {
    query <- "/1.1/favorites/destroy"
  } else {
    query <- "/1.1/favorites/create"
  }

  params <- list(id = status_id)
  TWIT_post(token, query, params)
}


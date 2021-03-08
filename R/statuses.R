#' Get tweets data for given statuses (status IDs).
#'
#' Returns data on up to 90,000 Twitter statuses. To return data on
#' more than 90,000 statuses, users must iterate through status IDs
#' whilst avoiding rate limits, which reset every 15 minutes.
#'
#' @inheritParams lookup_users
#' @param statuses User id or screen name of target user.
#' @seealso <https://developer.twitter.com/en/docs/tweets/post-and-engage/api-reference/get-statuses-lookup>
#' @examples
#'
#' \dontrun{
#'
#' ## create object containing status IDs
#' statuses <- c(
#'   "567053242429734913",
#'   "266031293945503744",
#'   "440322224407314432"
#' )
#'
#' ## lookup tweets data for given statuses
#' tw <- lookup_statuses(statuses)
#' tw
#'
#' }
#'
#' @return A tibble of tweets data.
#' @family tweets
#' @export
lookup_statuses <- function(statuses, parse = TRUE, token = NULL) {
  lookup_tweets(statuses = statuses, parse = parse, token = token)
}

#' @rdname lookup_statuses
#' @export
lookup_tweets <- function(statuses, parse = TRUE, token = NULL) {
  stopifnot(is.atomic(statuses))
  if (length(statuses) > 90000) {
    warning("number of statuses exceed max per token",
      "collecting data for first 90,000 ids")
    statuses <- statuses[1:90000]
  }
  
  chunks <- unname(split(statuses, (seq_along(statuses) - 1) %/% 100))
  results <- lapply(chunks, status_lookup_100, token = token)
  
  if (parse) {
    results <- tweets_with_users(results)
  }
  results
}

status_lookup_100 <- function(id, token = NULL) {
  stopifnot(length(id) <= 100)

  params <- list(
    id = paste(id, collapse = ","),
    tweet_mode = "extended",
    include_ext_alt_text = "true"
  )

  resp <- TWIT_post(token, "/1.1/statuses/lookup", params = params)
  from_js(resp)
}

#' get_timeline
#'
#' @description Returns timeline of tweets from a specified
#'   Twitter user.
#'
#' @param user Screen name or user id of target user.
#' @param count Numeric, number of tweets to return.
#' @param max_id Character, status_id from which returned tweets
#'   should be older than
#' @param token OAuth token (1.0 or 2.0). By default
#'   \code{token = NULL} fetches a non-exhausted token from
#'   an environment variable.
#' @param \dots Other arguments passed on to \code{make_url}.
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#' @importFrom dplyr bind_rows
#'
#' @return user ids
#' @export
get_timeline <- function(user, count = 200, max_id = NULL,
                         token = NULL, ...) {

  if (count < 200) {
    counter <- count
  } else {
    counter <- 200
  }

  if (count > 200) {
    count <- 200
  }

  if (is.list(user)) user <- unlist(user)

  if (length(user) > 1) {
    stop("too many users. Please provide users one at a time.", call. = FALSE)
  }

  params <- list(
    id_type = user,
    count = counter,
    max_id = max_id,
    ...)

  names(params)[1] <- .id_type(user)

  url <- make_url(restapi = TRUE, "statuses/user_timeline", params)

  token <- check_token(token, "statuses/user_timeline")

  res <- TWIT(get = TRUE, url, token)

  res <- from_js(res)

  tml_df <- tweets_df(res)

  tml_df
}

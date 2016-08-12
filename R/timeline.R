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

  tml <- vector("list", ceiling(count / 200))

  for (i in seq_along(tml)) {
    tml[[i]] <- tryCatch(timeline(
      user = user,
      count = count,
      token = token,
      max_id = max_id,
      ...),
      error = function(e) return(NULL))

    if (is.null(tml[[i]])) break

    count <- count - 200

    if (identical(tail(tml[[i]]$status_id, 1), max_id)) break

    max_id <- tail(tml[[i]]$status_id, 1)
  }

  bind_rows(tml)
}


timeline <- function(user, count = 200, max_id = NULL,
                         token = NULL, ...) {

  query <- "statuses/user_timeline"

  stopifnot(is.numeric(count), is.atomic(user), is.atomic(max_id))

  if (length(user) > 1) {
    stop("can only return tweets for one user at a time.", call. = FALSE)
  }

  if (count < 200) {
    count <- count
  } else {
    count <- 200
  }

  params <- list(
    id_type = user,
    count = count,
    max_id = max_id,
    ...)

  names(params)[1] <- .id_type(user)

  res <- TWIT(
    url = make_url(restapi = TRUE, query, params),
    config = check_token(token, query))

  tml_df <- tweets_df(from_js(res))

  tml_df
}


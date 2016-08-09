#' get_timeline
#'
#' @description Returns timeline of tweets from a specified
#'   Twitter user.
#'
#' @param user Screen name or user id of target user.
#' @param count Numeric, number of tweets to return.
#' @param token OAuth token (1.0 or 2.0). By default
#'   \code{token = NULL} fetches a non-exhausted token from
#'   an environment variable.
#' @param \dots Other arguments passed on to \code{make_url}.
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#' @importFrom dplyr bind_rows
#'
#' @return user ids
#' @export
get_timeline <- function(user, count = 200, token = NULL, ...) {

  if (count < 200) {
    counter <- count
  } else {
    counter <- 200
  }

  if (count > 36000) {
    count <- 36000
  }

  if (is.list(user)) user <- unlist(user)

  if (length(user) > 1) {
    stop("too many users. Please provide users one at a time.", call. = FALSE)
  }

  params <- list(
    id_type = user,
    count = counter,
    max_id = NULL,
    ...)

  names(params)[1] <- .id_type(user)

  url <- make_url(restapi = TRUE, "statuses/user_timeline", params)

  token <- check_token(token, "statuses/user_timeline")

  tml <- vector("list", ceiling(count / 200))

  for (i in seq_along(tml)) {

    if (all((i * 200) > count, i > 1)) {
      params$count <- count %% 200
      url <- make_url(restapi = TRUE, "statuses/user_timeline", params)
    }

    res <- from_js(TWIT(get = TRUE, url, token))

    tml[[i]] <- tweets_df(res)

    params$max_id <- tail(tml[[i]][["status_id"]], 1)

    url <- make_url(restapi = TRUE, "search/tweets", params)
  }

  tml <- bind_rows(tml)

  tml
}

#' followers_get
#'
#' @description Returns follower ids of specified Twitter user.
#'
#' @param user Screen name or user id of target user.
#' @param token OAuth token (1.0 or 2.0). By default
#'   \code{token = NULL} fetches a non-exhausted token from
#'   an environment variable.
#' @param page Default \code{page = -1} specifies first page of json
#'   results. Other pages specified via cursor values supplied by
#'   Twitter API response object.
#' @param stringify logical, indicating whether to return user ids
#'   as strings (some ids are too long to be read as numeric).
#'   Defaults to \code{TRUE}.
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#'
#' @return user ids
followers_get <- function(user, token = NULL, page = "-1",
                           stringify = TRUE) {

  params <- list(
    id_type = user,
    count = 5000,
    cursor = page,
    stringify = stringify)

  names(params)[1] <- .id_type(user)

  url <- make_url(restapi = TRUE, "followers/ids", params)

  if (is.null(token)) {
    token <- get_tokens()
    token <- fetch_tokens(token, "followers/ids")
  }

  resp <- TWIT(get = TRUE, url, token)

  flw <- from_js(resp)

  flw
}

#' get_followers
#'
#' @description Returns max followers per token
#' @param user Screen name or user id of target user.
#' @param token OAuth token (1.0 or 2.0). By default
#'   \code{token = NULL} fetches a non-exhausted token from
#'   an environment variable.
#' @param page Default \code{page = -1} specifies first page of json
#'   results. Other pages specified via cursor values supplied by
#'   Twitter API response object.
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#' @examples
#' \dontrun{
#' # get ids of users following the president of the US
#' pres <- get_followers(user = "potus")
#' pres
#'
#' # get ids of users following the Environmental Protection Agency
#' epa <- get_followers(user = "epa")
#' epa
#' }
#'
#' @return list of follower ids and next page value (presumably
#'   this would be used in loops extracting more than 75,000
#'   followers using either multiple tokens or by waiting out
#'   rate limits)
#' @export
get_followers <- function(user, token = NULL, page = "-1") {

  if (is.null(token)) {
    token <- get_tokens()
    token <- fetch_tokens(token, "followers/ids")
  }

  remaining <- rate_limit(token, "followers/ids")[, "remaining"]

  flw <- character()

  for (i in seq_len(remaining)) {

    flw_new <- followers_get(user, token, page)

    flw <- append(flw, getElement(flw_new, "ids"))

    page <- getElement(flw_new, "next_cursor_str")

    if (length(page) == 0) break
    if (page == 0) break
  }

  list(user_id = flw, page = page)
}

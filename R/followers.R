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

  token <- check_token(token, "followers/ids")

  resp <- TWIT(get = TRUE, url, token)

  flw <- from_js(resp)

  flw
}

#' get_followers
#'
#' @description Returns max followers per token
#' @param user Screen name or user id of target user.
#' @param parse Logical, indicating whether to return parsed
#'   vector or nested list (fromJSON) object. By default,
#'   \code{parse = TRUE} saves users from the time
#'   [and frustrations] associated with disentangling the Twitter
#'   API return objects.
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
get_followers <- function(user, n = 75000, stringify = TRUE, parse = TRUE,
                          max_id = NULL, token = NULL) {

  query <- "followers/ids"

  stopifnot(is.numeric(n), is.atomic(user), is.atomic(max_id))

  if (length(user) > 1) {
    stop("can only return friends for one user at a time.", call. = FALSE)
  }

  token <- check_token(token, query)

  params <- list(
    user_type = user,
    count = 5000,
    max_id = max_id,
    stringify = stringify)

  names(params)[1] <- .id_type(user)

  url <- make_url(
    query = query,
    param = params)

  fd <- scroller(url, n, token)

  fd <- fd[!sapply(fd, is.null)]

  if (parse) fd <- parse_followers(fd)

  fd
}

parse_followers <- function(x) {
  if (length(x) == 1) {
    x <- x[[1]][["ids"]]
  } else if (length(x > 1)) {
    x <- unlist(lapply(x, function(x) x[["users"]]))
  }
  x
}

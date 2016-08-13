#' get_friends
#'
#' @description Requests information from Twitter's REST API
#'   regarding a user's friend network (i.e., accounts followed
#'   by a user). To request information on followers of accounts
#'
#' @param user Screen name or user id of target user.
#' @param n Number of friends to return. For max return, enter
#'   \code{n = "all"} or \code{n = 75000} (max per token).
#' @param page Default \code{page = -1} specifies first page of json
#'   results. Other pages specified via cursor values supplied by
#'   Twitter API response object.
#' @param parse Logical, indicating whether to return parsed
#'   vector or nested list (fromJSON) object. By default,
#'   \code{parse = TRUE} saves you the time [and frustrations]
#'   associated with disentangling the Twitter API return objects.
#' @param token OAuth token (1.0 or 2.0). By default
#'   \code{token = NULL} fetches a non-exhausted token from
#'   an environment variable.
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#' @examples
#' \dontrun{
#' # get ids of users followed by the president of the US
#' pres <- get_friends(user = "potus")
#' pres
#'
#' # get ids of users followed by the Environmental Protection Agency
#' epa <- get_friends(user = "epa")
#' epa
#' }
#'
#' @return friends User ids for everyone a user follows.
#' @export
get_friends <- function(user, n = 75000, page = "-1", parse = TRUE,
  token = NULL) {

  query <- "friends/ids"

  if (n == "all") {
    n <- 75000
  }

  stopifnot(is_n(n), is.atomic(user), is.atomic(page))

  if (length(user) > 1) {
    stop("can only return friends for one user at a time.", call. = FALSE)
  }

  token <- check_token(token, query)

  n.times <- rate_limit(token, query)[["remaining"]]

  params <- list(
    user_type = user,
    count = 5000,
    cursor = page,
    stringify = TRUE)

  names(params)[1] <- .id_type(user)

  url <- make_url(
    query = query,
    param = params)

  f <- scroller(url, n, n.times, token)

  f <- f[!sapply(f, is.null)]

  if (parse) f <- parse_fs(f, n)

  f
}

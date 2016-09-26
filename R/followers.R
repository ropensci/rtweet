#' get_followers
#'
#' @description Returns max followers per token
#' @param user Screen name or user id of target user.
#' @param n Number of followers to return. For max return, enter
#'   \code{n = "all"} or \code{n = 75000} (max per token).
#' @param page Default \code{page = -1} specifies first page of json
#'   results. Other pages specified via cursor values supplied by
#'   Twitter API response object.
#' @param parse Logical, indicating whether to return parsed
#'   vector or nested list (fromJSON) object. By default,
#'   \code{parse = TRUE} saves you the time [and frustrations]
#'   associated with disentangling the Twitter API return objects.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find instructions
#'   on how to create tokens and setup an environment variable in the
#'   tokens vignette (in r, send \code{?tokens} to console).
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
#' @family ids
#' @export
get_followers <- function(user, n = 75000, page = "-1", parse = TRUE,
                          token = NULL) {

  query <- "followers/ids"

  if (n == "all") {
    n <- 75000
  }

  stopifnot(is_n(n),
    is.atomic(user),
    is.atomic(page),
    isTRUE(length(user) == 1))

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

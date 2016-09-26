#' get_favorites
#'
#' @description Returns the 20 most recent Tweets liked by
#'   the authenticating or specified user.
#' @param user Screen name or user id of target user.
#' @param n Specifies the number of records to retrieve. Must be
#'   less than or equal to 200; defaults to 3000, which is the max
#'   number of favorites returned per token. Due to suspended or
#'   deleted content, this function may return fewer tweets than
#'   the desired (n) number.
#' @param since_id Returns results with an status_id greater
#'   than (that is, more recent than) the specified status_id.
#'   There are limits to the number of tweets returned by the REST
#'   API. If the limit is hit, since_id is adjusted (by Twitter) to
#'   the oldest ID available.
#' @param max_id Returns results with status_id less (older) than or
#'   equal to (if hit limit) the specified status_id.
#' @param parse Logical, indicating whether to return parsed
#'   vector or nested list (fromJSON) object. By default,
#'   \code{parse = TRUE} saves you the time [and frustrations]
#'   associated with disentangling the Twitter API return objects.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find instructions
#'   on how to create tokens and setup an environment variable in the
#'   tokens vignette (in r, send \code{?tokens} to console).
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
#' @family tweets
#' @return Tweets data frame.
#' @export
get_favorites <- function(user, n = 3000, since_id = NULL,
  max_id = NULL, parse = TRUE, token = NULL) {

  query <- "favorites/list"

  if (n > 3000) {
    warning("n exceeds max favs returned per token. Setting n to 3000...",
      call. = FALSE)
    n <- 3000
  }

  stopifnot(is_n(n),
    is.atomic(user),
    isTRUE(length(user) == 1))

  token <- check_token(token, query)

  n.times <- rate_limit(token, query)[["remaining"]]
  if (n.times == 0L) stop("rate limit exceeded", call. = FALSE)

  params <- list(
    user_type = user,
    count = 200)

  names(params)[1] <- .id_type(user)

  url <- make_url(
    query = query,
    param = params)

  fav <- scroller(url, n, n.times, token)

  if (parse) {
    fav <- parser(fav, n)
    fav <- attr_tweetusers(fav[c("tweets", "users")])
  }

  fav
}

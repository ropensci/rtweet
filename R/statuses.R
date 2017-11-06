#' Get tweets data for given statuses (status IDs).
#'
#' Returns data on up to 90,000 Twitter statuses. To return data on
#' more than 90,000 statuses, users must iterate through status IDs
#' whilst avoiding rate limits, which reset every 15 minutes.
#'
#' @param statuses User id or screen name of target user.
#' @param parse Logical, indicating whether or not to parse
#'   return object into data frame(s).
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find
#'   instructions on how to create tokens and setup an environment
#'   variable in the tokens vignette (in r, send \code{?tokens} to
#'   console).
#' @seealso \url{https://developer.twitter.com/en/docs/tweets/post-and-engage/api-reference/get-statuses-lookup}
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
#' ## view users data for these statuses via users_data()
#' users_data(tw)
#'
#' }
#'
#' @return A tibble of tweets data.
#' @family tweets
#' @export
lookup_statuses <- function(statuses, parse = TRUE, token = NULL) {
  args <- list(statuses = statuses, parse = parse, token = token)
  do.call("lookup_statuses_", args)
}

#' @inheritParams lookup_statuses
#' @rdname lookup_statuses
#' @export
lookup_tweets <- function(statuses, parse = TRUE, token = NULL) {
  lookup_statuses(statuses, parse = parse, token = token)
}

lookup_statuses_ <- function(statuses,
                             token = NULL,
                             parse = TRUE) {
  stopifnot(is.atomic(statuses))
  if (length(statuses) > 90000) {
    warning("number of statuses exceed max per token",
      "collecting data for first 90,000 ids")
    statuses <- statuses[1:90000]
  }
  n.times <- ceiling(length(statuses) / 100)
  from <- 1
  twt <- vector("list", n.times)
  for (i in seq_len(n.times)) {
    to <- from + 99
    if (to > length(statuses)) {
      to <- length(statuses)
    }
    twt[[i]] <- .status_lookup(
      statuses[from:to],
      token, parse = parse)
    from <- to + 1
    if (from > length(statuses)) break
  }
  if (parse) {
    twt <- tweets_with_users(twt)
  }
  twt
}

.status_lookup <- function(statuses, token = NULL, parse) {
  query <- "statuses/lookup"
  if (is.list(statuses)) {
    statuses <- unlist(statuses)
  }
  stopifnot(is.atomic(statuses))
  if (length(statuses) > 100) {
    statuses <- statuses[1:100]
  }
  params <- list(
    id = paste(statuses, collapse = ","),
    tweet_mode = "extended")
  url <- make_url(
    query = query,
    param = params)
  token <- check_token(token, query = "statuses/lookup")
  resp <- TWIT(get = TRUE, url, token)
  from_js(resp)
}

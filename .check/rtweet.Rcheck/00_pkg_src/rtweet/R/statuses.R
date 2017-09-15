#' lookup_tweets
#'
#' @description Returns Twitter user data_frame object for
#'   specified user_ids or screen_names.
#'
#' @param statuses User id or screen name of target user.
#' @param token OAuth token (1.0 or 2.0). By default
#'   \code{token = NULL} fetches a non-exhausted token from
#'   an environment variable @describeIn tokens.
#' @param parse Logical, indicating whether or not to parse
#'   return object into data frame(s).
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#' @examples
#' \dontrun{
#' # lookup tweets data via status_id vector
#' statuses <- c("567053242429734913", "266031293945503744",
#'   "440322224407314432")
#' statuses <- lookup_statuses(statuses)
#' statuses
#'
#' # view users data for these statuses via tweets_data()
#' users_data(statuses)
#' }
#'
#' @return json response object (max is 18000 per token)
#' @family tweets
#' @aliases lookup_tweets
#' @export
lookup_statuses <- function(statuses, ...) {
  UseMethod("lookup_statuses")
}

lookup_statuses <- function(statuses,
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

  params <- list(id = paste(statuses, collapse = ","))

  url <- make_url(
    query = query,
    param = params)

  token <- check_token(token, query = "statuses/lookup")

  resp <- TWIT(get = TRUE, url, token)

  from_js(resp)
}

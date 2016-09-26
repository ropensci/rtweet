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
#'   return object into data frame(s)
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#' @examples
#' \dontrun{
#' # lookup vector of 1 or more user_id or screen_name
#' statuses <- c("potus", "hillaryclinton", "realdonaldtrump",
#'   "fivethirtyeight", "cnn", "espn", "twitter")
#'
#' twt_df <- lookup_statuses(statuses)
#' twt_df
#'
#' # view tweet data for these statuses via tweets_data()
#' tweets_data(twt_df)
#' }
#'
#' @return json response object (max is 18000 per token)
#' @family tweets
#' @export
lookup_statuses <- function(statuses, token = NULL, parse = TRUE) {

  if (is.list(statuses)) {
    statuses <- unlist(statuses)
  }

  if (length(statuses) > 18000) {
    statuses <- statuses[1:18000]
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
    twt <- parser(twt)
    twt <- attr_tweetusers(twt[c("tweets", "users")])
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

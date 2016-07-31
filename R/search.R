#' search_tweets
#'
#' @description Returns a collection of relevant Tweets matching a
#'   specified query.
#'
#' @param q Character vector search query of no greater than
#'   500 characters maximum.
#' @param count Numeric specifying the number of desired tweets to
#'   return per page. Defaults to maximum, which is 100.
#' @param type Character, specifies what type of search results
#'   you would prefer to receive. The current default is
#'   \code{type = "mixed"}.
#'   Valid values include \code{type = "mixed"} to include both
#'   popular and real time results in the response,
#'   \code{type = "recent"} to return only the most recent results
#'   in the response, and \code{type = "popular"} to return only
#'   the most popular results in the response.
#' @param token OAuth token (1.0 or 2.0). By default
#'   \code{token = NULL} fetches a non-exhausted token from
#'   an environment variable tokens.
#' @param \dots Futher arguments passed on to \code{make_url}.
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#' @examples
#' \dontrun{
#' # search for 1000 tweets mentioning Hillary Clinton
#' hrc <- search_tweets(q = "hillaryclinton", count = 1000)
#' hrc
#'
#' # search for 1000 tweets mentioning Donald Trump
#' djt <- search_tweets(q = "realdonaldtrump", count = 1000)
#' djt
#' }
#' @return Tweets data returned as a tibble data_frame
#' @details dplyr
#' @export
search_tweets <- function(q, count = 100, type = "mixed",
                          token = NULL, ...) {

  if (is.null(token)) {
    token <- get_tokens()
    token <- .fetch_tokens(token, "search/tweets")
  }

  params <- list(
    q = q,
    result_type = type,
    count = "100",
    cursor = "-1",
    ...)

  url <- make_url(
    restapi = TRUE,
    "search/tweets",
    param = params)

  tw_df <- dplyr::data_frame()
  nrows <- 0

  message("Searching for tweets...")

  while (nrows < count) {
    qresp <- TWIT(get = TRUE, url, config = token)
    qresp <- .from_js(qresp)

    tw_df <- dplyr::bind_rows(tw_df,
      parse_status(qresp$statuses))

    nrows <- nrow(tw_df)

    if (length(qresp$search_metadata$next_results) == 0) break
    if (qresp$search_metadata$next_results == 0) break

    url <- make_url(restapi = TRUE, "search/tweets",
      sub("[?]", "", qresp$search_metadata$next_results))
  }

  message(paste0("Collected ", nrows, " tweets!"))

  tw_df
}

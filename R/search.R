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
#'   All named arguments that do not match the above arguments
#'   (i.e., count, type, etc.) will be built into the request.
#'   To return only English language tweets, for example, use
#'   \code{lang = "en"}. Or, to exclude retweets, use
#'   \code{include_rts = FALSE}. For more options see Twitter's
#'   API documentation.
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#' @details Twitter API document recommends limiting searches to
#'   10 keywords and operators. Complex queries may also produce
#'   API errors preventing recovery of information related to
#'   the query.
#'   It should also be noted Twitter's search API does not consist
#'   of an index of all Tweets. At the time of searching, the
#'   search API index includes between only 6-9 days of Tweets.
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
#' @importFrom dplyr data_frame bind_rows
#' @export
search_tweets <- function(q, count = 100, type = "mixed",
  max_id = NULL, token = NULL, ...) {

  tw <- vector("list", ceiling(count / 100) + 1)

  message("Searching for tweets...")

  if (count > 1000) {
    message("It takes a little longer to collect this many tweets.")
  }

  for (i in seq_along(tw)) {
    r <- tryCatch(searchtweets(
      q = q,
      result_type = type,
      count = count,
      max_id = max_id,
      ...),
      error = function(e) return(NULL))

    if (is.null(r)) break

    tw[[i]] <- tweets_df(from_js(r))

    tw[[i]] <- tw[[i]][!duplicated(tw[[i]]), ]

    count <- count - nrow(tw[[i]])

    if (count <= 0) break

    if (identical(tail(tw[[i]]$status_id, 1), max_id)) break

    max_id <- tail(tw[[i]]$status_id, 1)
  }

  tw <- bind_rows(tw)

  message(paste0("Collected ", nrow(tw), " tweets!"))

  tw
}


searchtweets <- function(q, count = 100, type = "mixed",
  max_id = NULL, token = NULL, ...) {

  query <- "search/tweets"

  stopifnot(is.numeric(count), is.atomic(q), is.atomic(max_id))

  if (length(type) > 1) {
    stop("can only select one search type. Try type = 'mixed'.",
      call. = FALSE)
  }

  if (!tolower(type) %in% c("mixed", "recent", "popular")) {
    stop("invalid search type - must be mixed, recent, or popular.",
      call. = FALSE)
  }

  if (count < 100) {
    count <- count
  } else {
    count <- 100
  }

  params <- list(
    q = q,
    result_type = type,
    count = count,
    max_id = max_id,
    ...)

  tw <- TWIT(
    url = make_url(restapi = TRUE, query, params),
    config = check_token(token, query))

  tw
}




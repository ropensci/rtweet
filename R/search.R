#' search_tweets
#'
#' @description Returns two data frames (tweets data and users data)
#'   using a provided search query.
#'
#' @param q Character, search query of no greater than
#'   500 characters maximum.
#' @param n Numeric, specifying the total number of desired tweets to
#'   return. Defaults to 100. Maximum number of tweets returned from
#'   a single token is 18,000. See details for more information.
#' @param type Character, specifies what type of search results
#'   you would prefer to receive. The current default is
#'   \code{type = "mixed"}, which is a mix between the other two
#'   valid values \code{type = "recent"} and \code{type = "popular"}.
#' @param max_id Character, specifying the [oldest] status id beyond
#'   which results should resume returning.
#' @param parse Logical, indicating whether to return parsed
#'   (data.frames) or nested list (fromJSON) object. By default,
#'   \code{parse = TRUE} saves users from the time
#'   [and frustrations] associated with disentangling the Twitter
#'   API return objects.
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
#'
#'
#'   Number of tweets returned will often be less than what was
#'   specified by the user. This can happen because (a) the search
#'   query did not return many results (the search pool is already
#'   thinned out from the population of tweets to begin with) or
#'   (b) because you hit your rate limit for a given token. Even if
#'   the query has lots of hits and the rate limit should be able to
#'   max out at 18,000, the returned number of tweets may be lower,
#'   but that's only because the functions filter out duplicates
#'   (e.g., 18,000 tweets were actually returned, but 30 of them were
#'   removed because they were repeats).
#' @examples
#' \dontrun{
#' # search for 1000 tweets mentioning Hillary Clinton
#' hrc <- search_tweets(q = "hillaryclinton", n = 1000)
#'
#' # data frame where each observation (row) is a different tweet
#' hrc$tweets
#'
#' # data frame where each observation (row) is a different user
#' hrc$users
#'
#' # search for 1000 tweets mentioning Donald Trump
#' djt <- search_tweets(q = "realdonaldtrump", n = 1000)
#' djt$tweets
#' djt$users
#' }
#' @return List object with tweets and users each returned as
#'   tibble data_frame.
#' @export
search_tweets <- function(q, n = 100, type = "mixed", max_id = NULL,
  parse = TRUE, token = NULL, ...) {

  query <- "search/tweets"

  stopifnot(is_n(n), is.atomic(q), is.atomic(max_id))

  token <- check_token(token, query)

  n.times <- rate_limit(token, query)[["remaining"]]

  if (nchar(q) > 500) {
    stop("q cannot exceed 500 characters.", call. = FALSE)
  }

  if (length(type) > 1) {
    stop("can only select one search type. Try type = 'mixed'.",
      call. = FALSE)
  }

  if (!tolower(type) %in% c("mixed", "recent", "popular")) {
    stop("invalid search type - must be mixed, recent, or popular.",
      call. = FALSE)
  }

  params <- list(q = q,
    result_type = type,
    count = 100,
    max_id = max_id,
    ...)

  url <- make_url(
    query = query,
    param = params)

  message("Searching for tweets...")

  tw <- scroller(url, n, n.times, token)

  message(paste0("Collected ", nrow(tw), " tweets!"))

  if (parse) tw <- parser(tw, n)

  tw
}

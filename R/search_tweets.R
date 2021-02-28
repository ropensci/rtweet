#' Get tweets data on statuses identified via search query.
#'
#' Returns Twitter statuses matching a user provided search
#' query. ONLY RETURNS DATA FROM THE PAST 6-9 DAYS. To return more
#' than 18,000 statuses in a single call, set "retryonratelimit" to
#' TRUE.
#'
#' @inheritParams lookup_users
#' @param q Query to be searched, used to filter and select tweets to
#'   return from Twitter's REST API. Must be a character string not to
#'   exceed maximum of 500 characters. Spaces behave like boolean
#'   "AND" operator. To search for tweets containing at least one of
#'   multiple possible terms, separate each search term with spaces
#'   and "OR" (in caps). For example, the search `q =
#'   "data science"` looks for tweets containing both "data" and
#'   "science" located anywhere in the tweets and in any order.
#'   When "OR" is entered between search terms, `query =
#'   "data OR science"`, Twitter's REST API should return any tweet
#'   that contains either "data" or "science." It is also possible to
#'   search for exact phrases using double quotes. To do this, either
#'   wrap single quotes around a search query using double quotes,
#'   e.g., `q = '"data science"'` or escape each internal double
#'   quote with a single backslash, e.g., `q =
#'   "\"data science\""`.
#'
#' Some other useful query tips:
#'
#' \itemize{
#'   \item Exclude retweets via `"-filter:retweets"`
#'   \item Exclude quotes via `"-filter:quote"`
#'   \item Exclude replies via `"-filter:replies"`
#'   \item Filter (return only) verified via `"filter:verified"`
#'   \item Exclude verified via `"-filter:verified"`
#'   \item Get everything (firehose for free) via `"-filter:verified OR filter:verified"`
#'   \item Filter (return only) tweets with links to news articles via `"filter:news"`
#'   \item Filter (return only) tweets with media `"filter:media"`
#' }
#'
#' @param n Integer, specifying the total number of desired tweets to
#'   return. Defaults to 100. Maximum number of tweets returned from a
#'   single token is 18,000. To return more than 18,000 tweets, users
#'   are encouraged to set `retryonratelimit` to TRUE. See
#'   details for more information.
#' @param type Character string specifying which type of search
#'   results to return from Twitter's REST API. The current default is
#'   `type = "recent"`, other valid types include `type =
#'   "mixed"` and `type = "popular"`.
#' @param geocode Geographical limiter of the template
#'   "latitude,longitude,radius" e.g., `geocode =
#'   "37.78,-122.40,1mi"`.
#' @param include_rts Logical, indicating whether to include retweets
#'   in search results. Retweets are classified as any tweet generated
#'   by Twitter's built-in "retweet" (recycle arrows) function. These
#'   are distinct from quotes (retweets with additional text provided
#'   from sender) or manual retweets (old school method of manually
#'   entering "RT" into the text of one's tweets).
#' @param max_id Character, returns results with an ID less
#'   than (that is, older than) or equal to `max_id`.  Especially
#'   useful for large data returns that require multiple iterations
#'   interrupted by user time constraints. For searches exceeding
#'   18,000 tweets, users are encouraged to take advantage of rtweet's
#'   internal automation procedures for waiting on rate limits by
#'   setting `retryonratelimit` argument to TRUE.  It some cases,
#'   it is possible that due to processing time and rate limits,
#'   retrieving several million tweets can take several hours or even
#'   multiple days. In these cases, it would likely be useful to
#'   leverage `retryonratelimit` for sets of tweets and
#'   `max_id` to allow results to continue where previous efforts
#'   left off.
#' @param retryonratelimit Logical indicating whether to wait and
#'   retry when rate limited. This argument is only relevant if the
#'   desired return (n) exceeds the remaining limit of available
#'   requests (assuming no other searches have been conducted in the
#'   past 15 minutes, this limit is 18,000 tweets). Defaults to false.
#'   Set to TRUE to automate process of conducting big searches (i.e.,
#'   n > 18000). For many search queries, esp. specific or specialized
#'   searches, there won't be more than 18,000 tweets to return. But
#'   for broad, generic, or popular topics, the total number of tweets
#'   within the REST window of time (7-10 days) can easily reach the
#'   millions.
#' @param verbose Logical, indicating whether or not to include output
#'   processing/retrieval messages. Defaults to TRUE. For larger
#'   searches, messages include rough estimates for time remaining
#'   between searches. It should be noted, however, that these time
#'   estimates only describe the amount of time between searches and
#'   not the total time remaining. For large searches conducted with
#'   `retryonratelimit` set to TRUE, the estimated retrieval time
#'   can be estimated by dividing the number of requested tweets by
#'   18,000 and then multiplying the quotient by 15 (token reset
#'   time, in minutes).
#' @param ... Further arguments passed as query parameters in request
#'   sent to Twitter's REST API. To return only English language
#'   tweets, for example, use `lang = "en"`. For more options see
#'   Twitter's API documentation.
#' @seealso
#'   <https://developer.twitter.com/en/docs/tweets/search/api-reference/get-search-tweets>
#' @details Twitter API documentation recommends limiting searches to
#'   10 keywords and operators. Complex queries may also produce API
#'   errors preventing recovery of information related to the query.
#'   It should also be noted Twitter's search API does not consist of
#'   an index of all Tweets. At the time of searching, the search API
#'   index includes between only 6-9 days of Tweets.
#'
#'
#'   Number of tweets returned will often be less than what was
#'   specified by the user. This can happen because (a) the search
#'   query did not return many results (the search pool is already
#'   thinned out from the population of tweets to begin with),
#'   (b) because user hitting rate limit for a given token, or (c)
#'   of recent activity (either more tweets, which affect pagination
#'   in returned results or deletion of tweets). To return more than
#'   18,000 tweets in a single call, users must set
#'   `retryonratelimit` argument to true. This method relies on
#'   updating the `max_id` parameter and waiting for token rate
#'   limits to refresh between searches. As a result, it is possible
#'   to search for 50,000, 100,000, or even 10,000,000 tweets, but
#'   these searches can take hours or even days. At these durations,
#'   it would not be uncommon for connections to timeout. Users are
#'   instead encouraged to breakup data retrieval into smaller chunks
#'   by leveraging `retryonratelimit` and then using the
#'   status_id of the oldest tweet as the `max_id` to resume
#'   searching where the previous efforts left off.
#'
#' @examples
#'
#' \dontrun{
#'
#' ## search for 1000 tweets mentioning Hillary Clinton
#' hrc <- search_tweets(q = "hillaryclinton", n = 1000)
#'
#' ## data frame where each observation (row) is a different tweet
#' hrc
#'
#' ## users data also retrieved. can access it via users_data()
#' users_data(hrc)
#'
#' ## search for 1000 tweets in English
#' djt <- search_tweets(q = "realdonaldtrump", n = 1000, lang = "en")
#'
#' ## preview tweets data
#' djt
#'
#' ## preview users data
#' users_data(djt)
#'
#' ## exclude retweets
#' rt <- search_tweets("rstats", n = 500, include_rts = FALSE)
#'
#' ## perform search for lots of tweets
#' rt <- search_tweets(
#'   "trump OR president OR potus", n = 100000,
#'   retryonratelimit = TRUE
#' )
#'
#' ## plot time series of tweets frequency
#' ts_plot(rt, by = "mins")
#'
#' ## make multiple independent search queries
#' ds <- Map(
#'   "search_tweets",
#'   c("\"data science\"", "rstats OR python"),
#'   n = 1000
#' )
#'
#' ## bind by row whilst preserving users data
#' ds <- do_call_rbind(ds)
#'
#' ## preview tweets data
#' ds
#'
#' ## preview users data
#' users_data(ds)
#'
#' }
#'
#' @return List object with tweets and users each returned as a
#'   data frame.
#' @family tweets
#' @export
search_tweets <- function(q, n = 100,
                          type = c("mixed", "recent", "popular"),
                          include_rts = TRUE,
                          geocode = NULL,
                          max_id = NULL,
                          parse = TRUE,
                          token = NULL,
                          retryonratelimit = FALSE,
                          verbose = TRUE,
                          ...) {
  
  params <- search_params(q, 
    n = n,
    type = type,
    include_rts = include_rts,
    geocode = geocode,
    max_id = max_id,
    ...
  )
  
  result <- TWIT_paginate_max_id(token, "search/tweets", params,
    get_max_id = function(x) x$statuses$id_str,
    n = n,
    page_size = 100,
    parse = parse
  )

  if (parse) {
    result <- tweets_with_users(result)
  }
  result
}

search_params <- function(q, n, 
                          type = c("mixed", "recent", "popular"),
                          include_rts = TRUE,
                          geocode = NULL,
                          max_id = NULL,
                          ...) {
  if (missing(q) && !is.null(geocode)) {
    q <- ""
  }
  stopifnot(is_n(n), is.atomic(q), length(q) == 1L, is.atomic(max_id))
  type <- arg_match(type)
  
  ## validate query length–char count might not always be same here as with 
  ## Twitter, so set this to 600 and let Twitter reject others
  if (nchar(q) > 600) {
    stop("q cannot exceed 500 characters.", call. = FALSE)
  }
  if (!include_rts) {
    q <- paste0(q, " -filter:retweets")
  }

  if (!is.null(geocode) && inherits(geocode, "coords")) {
    mls1 <- abs(geocode$box[2] - geocode$box[4]) * 69
    mls2 <- abs(geocode$box[1] - geocode$box[3]) * 
      (69 - abs(.093 * geocode$point[1])^2)
    mls <- (mls1/1.8 + mls2/1.8) / 1.8
    mls <- round(mls, 3)
    geocode <- paste0(paste(geocode$point, collapse = ","), ",", mls, "mi")
  }
  
  list(
    q = q,
    result_type = type,
    max_id = max_id,
    tweet_mode = "extended",
    include_ext_alt_text = "true",
    geocode = geocode,
    ...
  )
}



#' Search tweets (vectorized)
#'
#' search_tweets2 Passes all arguments to search_tweets. Returns data from
#' one OR MORE search queries.
#'
#' @return A tbl data frame with additional "query" column.
#' @rdname search_tweets
#' @examples
#'
#' \dontrun{
#'
#' ## search using multilple queries
#' st2 <- search_tweets2(
#'   c("\"data science\"", "rstats OR python"),
#'   n = 500
#' )
#'
#' ## preview tweets data
#' st2
#'
#' ## preview users data
#' users_data(st2)
#'
#' ## check breakdown of results by search query
#' table(st2$query)
#'
#' }
#'
#' @export
search_tweets2 <- function(...) {
  dots <- match_fun(list(...), "search_tweets")
  q <- dots[["q"]]
  dots[["q"]] <- NULL
  ## is parse = TRUE?
  parse <- dots[["parse"]]
  ## search for each string in column of queries
  rt <- Map("search_tweets", q, MoreArgs = dots)
  ## if parse is false, return rt
  if (!parse) {
    return(rt)
  }
  ## deal with queries that returned zero tweets
  kp <- lengths(rt) > 0L
  if (sum(kp, na.rm = TRUE) == 0L) return(data.frame())
  rt <- rt[kp]
  q <- q[kp]
  ## add query variable to data frames
  rt <- Map("add_var", rt, query = q)
  ## merge users data into one data frame
  do_call_rbind(rt)
}

add_var <- function(x, ...) {
  dots <- list(...)
  if (!is.null(names(dots))) {
    varname <- names(dots)
  } else {
    varname <- deparse(substitute(...))
  }
  x[[varname]] <- unlist(dots, use.names = FALSE)
  x
}

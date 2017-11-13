#' Get tweets data on statuses identified via search query.
#'
#' Returns Twitter statuses matching a user provided search
#' query. ONLY RETURNS DATA FROM THE PAST 6-9 DAYS. To return more
#' than 18,000 statuses in a single call, set "retryonratelimit" to
#' TRUE.
#'
#' @param q Query to be searched, used to filter and select tweets to
#'   return from Twitter's REST API. Must be a character string not to
#'   exceed maximum of 500 characters. Spaces behave like boolean
#'   "AND" operator. To search for tweets containing at least one of
#'   multiple possible terms, separate each search term with spaces
#'   and "OR" (in caps). For example, the search \code{q =
#'   "data science"} looks for tweets containing both "data" and
#'   "science" anywhere located anywhere in the tweets and in any
#'   order. When "OR" is entered between search terms, \code{query =
#'   "data OR science"}, Twitter's REST API should return any tweet
#'   that contains either "data" or "science." It is also possible to
#'   search for exact phrases using double quotes. To do this, either
#'   wrap single quotes around a search query using double quotes,
#'   e.g., \code{q = '"data science"'} or escape each internal double
#'   quote with a single backslash, e.g., \code{q =
#'   "\"data science\""}.
#' @param n Integer, specifying the total number of desired tweets to
#'   return. Defaults to 100. Maximum number of tweets returned from a
#'   single token is 18,000. To return more than 18,000 tweets, users
#'   are encouraged to set \code{retryonratelimit} to TRUE. See
#'   details for more information.
#' @param type Character string specifying which type of search
#'   results to return from Twitter's REST API. The current default is
#'   \code{type = "recent"}, other valid types include \code{type =
#'   "mixed"} and \code{type = "popular"}.
#' @param geocode Geographical limiter of the template
#'   "latitude,longitude,radius" e.g., \code{geocode =
#'   "37.78,-122.40,1mi"}.
#' @param include_rts Logical, indicating whether to include retweets
#'   in search results. Retweets are classified as any tweet generated
#'   by Twitter's built-in "retweet" (recycle arrows) function. These
#'   are distinct from quotes (retweets with additional text provided
#'   from sender) or manual retweets (old school method of manually
#'   entering "RT" into the text of one's tweets).
#' @param max_id Character string specifying the [oldest] status id
#'   beyond which search results should resume returning.  Especially
#'   useful large data returns that require multiple iterations
#'   interrupted by user time constraints. For searches exceeding
#'   18,000 tweets, users are encouraged to take advantage of rtweet's
#'   internal automation procedures for waiting on rate limits by
#'   setting \code{retryonratelimit} argument to TRUE.  It some cases,
#'   it is possible that due to processing time and rate limits,
#'   retrieving several million tweets can take several hours or even
#'   multiple days. In these cases, it would likely be useful to
#'   leverage \code{retryonratelimit} for sets of tweets and
#'   \code{max_id} to allow results to continue where previous efforts
#'   left off.
#' @param parse Logical, indicating whether to return parsed
#'   data.frame, if true, or nested list, if false. By default,
#'   \code{parse = TRUE} saves users from the wreck of time and
#'   frustration associated with disentangling the nasty nested list
#'   returned from Twitter's API. As Twitter's APIs are subject to
#'   change, this argument would be especially useful when changes to
#'   Twitter's APIs affect performance of internal parsers. Setting
#'   \code{parse = FALSE} also ensures the maximum amount of possible
#'   information is returned. By default, the rtweet parse process
#'   returns nearly all bits of information returned from
#'   Twitter. However, users may occasionally encounter new or
#'   omitted variables. In these rare cases, the nested list object
#'   will be the only way to access these variables.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find
#'   instructions on how to create tokens and setup an environment
#'   variable in the tokens vignette (in r, send \code{?tokens} to
#'   console).
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
#'   \code{retryonratelimit} set to TRUE, the estimated retrieval time
#'   can be estimated by dividing the number of requested tweets by
#'   18,000 and then multiplying the quotient by 15 (token reset
#'   time, in minutes).
#' @param ... Further arguments passed as query parameters in request
#'   sent to Twitter's REST API. To return only English language
#'   tweets, for example, use \code{lang = "en"}. For more options see
#'   Twitter's API documentation.
#' @seealso
#'   \url{https://developer.twitter.com/en/docs/tweets/search/api-reference/get-search-tweets}
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
#'   \code{retryonratelimit} argument to true. This method relies on
#'   updating the \code{max_id} parameter and waiting for token rate
#'   limits to refresh between searches. As a result, it is possible
#'   to search for 50,000, 100,000, or even 10,000,000 tweets, but
#'   these searches can take hours or even days. At these durations,
#'   it would not be uncommon for connections to timeout. Users are
#'   instead encouraged to breakup data retrieval into smaller chunks
#'   by leveraging \code{retryonratelimit} and then using the
#'   status_id of the oldest tweet as the \code{max_id} to resume
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
                          type = "recent",
                          include_rts = TRUE,
                          geocode = NULL,
                          max_id = NULL,
                          parse = TRUE,
                          token = NULL,
                          retryonratelimit = FALSE,
                          verbose = TRUE,
                          ...) {
  if (missing(q) && !is.null(geocode)) {
    q <- ""
  }
  args <- list(
    q = q,
    n = n,
    type = type,
    include_rts = include_rts,
    geocode = geocode,
    max_id = max_id,
    parse = parse,
    token = token,
    retryonratelimit = retryonratelimit,
    verbose = verbose,
    ...
  )
  do.call("search_tweets_", args)
}


search_tweets_ <- function(q = "",
                           n = 100,
                           type = "recent",
                           max_id = NULL,
                           geocode = NULL,
                           include_rts = TRUE,
                           parse = TRUE,
                           token = NULL,
                           retryonratelimit = FALSE,
                           verbose = TRUE,
                           ...) {

  ## check token and get rate limit data
  token <- check_token(token, "search/tweets")
  rtlimit <- rate_limit(token, "search/tweets")
  remaining <- rtlimit[["remaining"]] * 100
  reset <- rtlimit[["reset"]]
  reset <- as.numeric(reset, "secs")

  if (n <= remaining || !retryonratelimit) {
    rt <- .search_tweets(
      q = q, n = n,
      type = type,
      geocode = geocode,
      max_id = max_id,
      include_rts = include_rts,
      parse = parse,
      token = token,
      verbose = verbose,
      ...)
  } else {
    if (identical(remaining, 0)) {
      ntimes <- ceiling((n - remaining) / 18000)
    } else {
      ntimes <- ceiling((n - remaining) / 18000) + 1
    }
    rt <- vector("list", ntimes)
    maxid <- max_id
    for (i in seq_len(ntimes)) {
      ## if rate limited (exhausted token)
      if (any(identical(remaining, 0), isTRUE(remaining < 10))) {
        message(paste0(
          "retry on rate limit...\n",
          "waiting about ",
          round(reset / 60, 0),
          " minutes..."))
        Sys.sleep(reset + 2)
        remaining <- 180 * 100
      }
      rt[[i]] <- tryCatch(
        .search_tweets(
          q = q, n = remaining,
          check = FALSE,
          type = type,
          geocode = geocode,
          max_id = maxid,
          include_rts = include_rts,
          parse = parse,
          token = token,
          verbose = verbose,
          ...),
        error = function(e) return(NULL))
      ## break if error
      if (is.null(rt[[i]])) break
      ## break if final i
      if (i == ntimes) break
      if (parse) {
        ## get next maxid
        maxid.new <- rt[[i]][["status_id"]][[NROW(rt[[i]])]]
      } else {
        maxid.new <- return_last(unlist(go_get_var(rt[[1]], "statuses", "id")))
      }
      ## break if new maxid is null, empty, or unchanged
      if (any(is.null(maxid.new),
        identical(length(maxid.new), 0L),
        identical(maxid, maxid.new))) break
      ## update maxid value
      maxid <- maxid.new
      ## refresh rate limit data
      rtlimit <- rate_limit(token, "search/tweets")
      remaining <- rtlimit[["remaining"]] * 100
      reset <- rtlimit[["reset"]]
      reset <- as.numeric(reset, "secs")
    }
    ## get users data if applicable
    users <- do.call("rbind", lapply(rt, users_data))
    rt <- do.call("rbind", rt)
    attr(rt, "users") <- users
  }
  rt
}


.search_tweets <- function(q,
                           n = 100,
                           check = FALSE,
                           geocode = NULL,
                           type = "recent",
                           max_id = NULL,
                           include_rts = TRUE,
                           parse = TRUE,
                           token = NULL,
                           verbose = TRUE,
                           ...) {
  ## path name
  query <- "search/tweets"
  ## validate
  stopifnot(is_n(n), is.atomic(q), is.atomic(max_id))
  ## number of loops
  n.times <- ceiling(n / 100)
  if (n < 100) {
    count <- n
  } else {
    count <- 100
  }
  ## validate query length
  if (nchar(q) > 500) {
    stop("q cannot exceed 500 characters.", call. = FALSE)
  }
  ## only select one type
  if (length(type) > 1) {
    stop("can only select one search type. Try type = 'recent'.",
         call. = FALSE)
  }
  if (!isTRUE(tolower(type) %in% c("mixed", "recent", "popular"))) {
    stop("invalid search type - must be mixed, recent, or popular.",
         call. = FALSE)
  }
  ## if no retweets add filter to query
  if (!include_rts) q <- paste0(q, " -filter:retweets")
  ## geocode prep
  if (!is.null(geocode)) {

    if (inherits(geocode, "coords")) {
      mls1 <- abs(geocode$box[2] - geocode$box[4]) * 69
      mls2 <- abs(
        geocode$box[1] - geocode$box[3]) * (69 - abs(.093 * geocode$point[1])^2)
      mls <- (mls1/1.8 + mls2/1.8) / 1.8
      mls <- round(mls, 3)
      geocode <- paste0(paste(geocode$point, collapse = ","), ",", mls, "mi")
    }
  }
  ## make params list
  params <- list(
    q = q,
    result_type = type,
    count = count,
    max_id = max_id,
    tweet_mode = "extended",
    geocode = geocode,
    ...)
  ## make url
  url <- make_url(
    query = query,
    param = params)

  if (verbose) {
    message("Searching for tweets...")
    if (n > 10000) message("This may take a few seconds...")
  }
  tw <- scroller(url, n, n.times, type = "search", token)
  if (parse) {
    tw <- tweets_with_users(tw)
  }
  if (verbose) {
    message("Finished collecting tweets!")
  }
  tw
}



#' Search tweets (vectorized)
#'
#' search_tweets2 Passes all arguments to search_tweets. Returns data from
#' one OR MORE search queries.
#'
#' @inheritParams search_tweets.
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




next_id <- function(df) {
  if (!all(c("created_at", "status_id") %in% names(df))) {
    stop("wrong data frame - function requires tweets data")
  }
  df <- df[!is.na(df$status_id), ]
  df <- df[order(df$created_at), ]
  df$status_id[1]
}

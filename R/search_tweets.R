#' Get tweets data on statuses identified via search query.
#'
#' Returns Twitter statuses matching a user provided search
#' query. ONLY RETURNS DATA FROM THE PAST 6-9 DAYS. 
#'
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
#' @inheritParams TWIT_paginate_max_id
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
#' @param ... Further arguments passed as query parameters in request
#'   sent to Twitter's REST API. To return only English language
#'   tweets, for example, use `lang = "en"`. For more options see
#'   Twitter's API documentation.
#' @details Twitter API documentation recommends limiting searches to
#'   10 keywords and operators. Complex queries may also produce API
#'   errors preventing recovery of information related to the query.
#'   It should also be noted Twitter's search API does not consist of
#'   an index of all Tweets. At the time of searching, the search API
#'   index includes between only 6-9 days of Tweets.
#' @examples
#' if (auth_has_default()) {
#' tweets <- search_tweets("weather")
#' tweets
#' 
#' # data about the users who made those tweets
#' users_data(tweets)
#' 
#' # Retrieve all the tweets made since the previous request
#' # (there might not be any if people aren't tweeting about the weather)
#' newer <- search_tweets("weather", since_id = tweets)
#' # Retrieve tweets made before the previous request
#' older <- search_tweets("weather", max_id = tweets)
#' 
#' # Restrict to English only, and ignore retweets
#' tweets2 <- search_tweets("weather", lang = "en", include_rts = FALSE)
#' }
#' @return List object with tweets and users each returned as a
#'   data frame.
#' @family tweets
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/v1/tweets/search/api-reference/get-search-tweets>
search_tweets <- function(q, n = 100,
                          type = c("mixed", "recent", "popular"),
                          include_rts = TRUE,
                          geocode = NULL,
                          since_id = NULL,
                          max_id = NULL,
                          parse = TRUE,
                          token = NULL,
                          retryonratelimit = NULL,
                          verbose = TRUE,
                          ...) {
  
  params <- search_params(q, 
    type = type,
    include_rts = include_rts,
    geocode = geocode,
    ...
  )
  
  result <- TWIT_paginate_max_id(token, "/1.1/search/tweets", params,
    get_id = function(x) x$statuses$id_str,
    page_size = 100,
    n = n,
    since_id = since_id,
    max_id = max_id,
    retryonratelimit = retryonratelimit,
    verbose = verbose
  )

  if (parse) {
    tweets <- lapply(result, "[[", "statuses")
    result <- tweets_with_users(tweets)
    result$created_at <- format_date(result$created_at)
  }
  result
}

search_params <- function(q,
                          type = c("mixed", "recent", "popular"),
                          include_rts = TRUE,
                          geocode = NULL,
                          max_id = NULL,
                          ...) {
  if (missing(q) && !is.null(geocode)) {
    q <- ""
  }
  stopifnot(is.atomic(q), length(q) == 1L, is.atomic(max_id))
  type <- arg_match(type)
  
  ## validate query length–char count might not always be same here as with 
  ## Twitter, so set this to 600 and let Twitter reject others
  if (nchar(q) > 600) {
    stop("q cannot exceed 600 characters.", call. = FALSE)
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
#' if (auth_has_default()) {
#'
#' ## search using multiple queries
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


match_fun <- function(dots, fun) {
  rfuns <- names(formals(fun))
  nms <- match(names(dots), rfuns)
  nms[names(dots) != ""] <- names(dots)[names(dots) != ""]
  is_na <- function(x) is.na(x) | x == "NA"
  nms[is_na(nms) & names(dots) == ""] <- names(
    formals(fun))[which(is_na(nms) & names(dots) == "")]
  names(dots) <- nms
  names(dots)[is.na(names(dots))] <- ""
  fmls <- formals(fun)
  dotsdots <- dots[!names(dots) %in% names(fmls)]
  dots <- dots[names(dots) %in% names(fmls)]
  fmls <- fmls[!names(fmls) %in% names(dots) & names(fmls) != "..."]
  c(dots, fmls, dotsdots)
}

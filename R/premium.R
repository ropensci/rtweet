#' Premium Twitter searches
#'
#' Search 30day or fullarchive premium products. There is a limit of 5000 tweets
#'  and 25000 for the fullarchive and 30day endpoints respectively. In addition,
#'  there are some limits in the number of requests that are possible on a
#'  certain amount of time, this have already been taken into account.
#'  See the info provided by Twitter and the "Developer Account" section.
#'
#' Note: The `env_name` must match the ones you set up for the token you are using.
#' `r lifecycle::badge("deprecated")`
#'
#' @inheritParams TWIT_paginate_max_id
#' @inheritParams stream
#' @param q Search query on which to match/filter tweets. See details for
#'   information about available search operators.
#' @param continue A character string with the next results of a query. You
#' must make the exact same query as the original, including `q`, `toDate`,
#' and `fromDate`.
#' @param premium A logical value if the environment is paid (TRUE) or
#' sandboxed, the default (FALSE). It limits the number of results retrieved so the number
#' of API queries needed to retrieve `n` results.
#' @param fromDate Oldest date-time (YYYYMMDDHHMM) from which tweets should be
#'   searched for.
#' @param toDate Newest date-time (YYYYMMDDHHMM) from which tweets should be
#'   searched for.
#' @param env_name Name/label of developer environment to use for the search.
#' @param safedir Name of directory to which each response object should be
#'   saved. If the directory doesn't exist, it will be created. If NULL (the
#'   default) then a dir will be created in the current working directory. To
#'   override/deactivate safedir set this to FALSE.
#'
#' @section Developer Account:
#' Users must have an approved developer account and an active/labeled
#' environment to access Twitter's premium APIs. For more information, to check
#' your current Subscriptions and Dev Environments, or to apply for a developer
#' account visit <https://developer.twitter.com>.
#'
#' @section Search operators:
#' *Note: Bolded operators ending with a colon should be immediately
#' followed by a word or quoted phrase (if appropriate)–e.g.,* `lang:en`
#'
#' @section Keyword:
#' \itemize{
#'   \item **""**           ~~ match exact phrase
#'   \item **#**               ~~ hashtag
#'   \item **@@**               ~~ at mentions)
#'   \item **url:**            ~~ found in URL
#'   \item **lang:**           ~~ language of tweet
#' }
#'
#' @section Accounts of interest:
#' \itemize{
#'   \item **from:**           ~~ authored by
#'   \item **to:**             ~~ sent to
#'   \item **retweets_of:**    ~~ retweet author
#' }
#'
#' @section Tweet attributes:
#' \itemize{
#'   \item **is:retweet**      ~~ only retweets
#'   \item **has:mentions**    ~~ uses mention(s)
#'   \item **has:hashtags**    ~~ uses hashtags(s)
#'   \item **has:media**       ~~ includes media(s)
#'   \item **has:videos**      ~~ includes video(s)
#'   \item **has:images**      ~~ includes image(s)
#'   \item **has:links**       ~~ includes URL(s)
#'   \item **is:verified**     ~~ from verified accounts
#' }
#'
#' @section Geospatial:
#' \itemize{
#'   \item **bounding_box:\[west_long south_lat east_long north_lat\]** ~~ lat/long coordinates box
#'   \item **point_radius:\[lon lat radius\]** ~~ center of search radius
#'   \item **has:geo**           ~~ uses geotagging
#'   \item **place:**            ~~ by place
#'   \item **place_country:**    ~~ by country
#'   \item **has:profile_geo**   ~~ geo associated with profile
#'   \item **profile_country:**  ~~ country associated with profile
#'   \item **profile_region:**   ~~ region associated with profile
#'   \item **profile_locality:** ~~ locality associated with profile
#' }
#'
#' @return A tibble data frame of Twitter data.
#' @family premium endpoints
#' @seealso [tweet_search_recent()], [tweet_search_all()], [`rtweet-deprecated`]
#' @export
search_fullarchive <- function(q, n = 100, fromDate = NULL, toDate = NULL,
                               continue = NULL,
                               env_name = NULL, premium = FALSE,
                               safedir = NULL, parse = TRUE, token = NULL) {

  search_premium("fullarchive",
    q = q,
    n = n,
    fromDate = fromDate,
    toDate = toDate,
    env_name = env_name,
    continue = continue,
    premium = premium,
    safedir = safedir,
    parse = parse,
    token = token
  )

}

#' @rdname search_fullarchive
#' @export
search_30day <- function(q, n = 100, fromDate = NULL, toDate = NULL,
                         env_name = NULL,
                         continue = NULL,  premium = FALSE,
                         safedir = NULL,
                         parse = TRUE,
                         token = NULL) {

  search_premium("30day",
    q = q,
    n = n,
    fromDate = fromDate,
    toDate = toDate,
    env_name = env_name,
    continue = continue,
    premium = premium,
    safedir = safedir,
    parse = parse,
    token = token
  )
}


search_premium <- function(product, q, n = NULL, fromDate = NULL, toDate = NULL,
                          env_name = NULL, continue = NULL, premium = FALSE, safedir = NULL,
                          parse = TRUE,
                          token = NULL) {

  if (is.null(env_name)) {
    stop("Must provide dev environment name")
  }
  if (!is.null(safedir)) {
    stop("`safedir` temporarily not supported")
  }

  if (!is_logical(premium)) {
    stop("premium must be either TRUE or FALSE.", call. = FALSE)
  }

  params <- list(query = q,
    maxResults = n,
    fromDate = format_from_to_date(fromDate),
    # tag = ?? Not sure how to support tags or how they are used.
    toDate = format_from_to_date(toDate)
  )

  api <- paste0("/1.1/tweets/search/", product, "/", env_name)
  result <- TWIT_paginate_premium(token, api, params, n = n, cursor = continue,
                                  page_size = if (premium) 500 else 100)

  if (parse) {
    cursor <- attr(result, "next")
    result <- tweets_with_users(result)
    result$created_at <- format_date(result$created_at)
    attr(result, "next") <- cursor
  }
  result
}

format_from_to_date <- function(x = NULL) {
  if (is.null(x)) {
    return(NULL)
  }
  if (length(x) > 1L) {
    stop("Can only provide one value to fromDate/toDate", call. = FALSE)
  }
  if (is.character(x) && grepl("-", x) && nchar(x) > 11) {
    x <- as.POSIXct(x)
  }
  if (is.character(x) && grepl("-", x)) {
    x <- as.Date(x)
  }
  if (inherits(x, "Date")) {
    x <- as.POSIXct(x)
  }
  if (inherits(x, "POSIXct")) {
    x <- format(x, "%Y%m%d%H%M")
  }
  x
}

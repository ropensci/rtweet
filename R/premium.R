#' Premium twitter searches
#'
#' Search 30day or fullarchive products
#'
#' @param q Search query on which to match/filter tweets. See details for
#'   information about available search operators.
#' @param n Number of tweets to return; it is best to set this number in
#'   intervals of 100 for the '30day' API and either 100 (for sandbox) or 500
#'   (for paid) for the 'fullarchive' API. Default is 100.
#' @param fromDate Oldest date-time (YYYYMMDDHHMM) from which tweets should be
#'   searched for.
#' @param toDate Newest date-time (YYYYMMDDHHMM) from which tweets should be
#'   searched for.
#' @param env_name Name/label of developer environment to use for the search.
#' @param safedir Name of directory to which each response object should be
#'   saved. If the directory doesn't exist, it will be created. If NULL (the
#'   default) then a dir will be created in the current working directory. To
#'   override/deactivate safedir set this to FALSE.
#' @inheritParams lookup_users
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
#' @return A tibble data frame of Twitter data
#' @examples
#'
#' \dontrun{
#' ## search fullarchive for up to 300 rstats tweets sent in Jan 2014
#' rt <- search_fullarchive("#rstats", n = 300, env_name = "research",
#'   fromDate = "201401010000", toDate = "201401312359")
#'
#' toDate <- format(Sys.time() - 60 * 60 * 24 * 7, "%Y%m%d%H%M")
#'
#' ## search 30day for up to 300 rstats tweets sent before the last week
#' rt <- search_30day("#rstats", n = 300,
#'   env_name = "research", toDate = toDate)
#' }
#'
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/premium/search-api/api-reference/premium-search>
search_fullarchive <- function(q, n = 100, fromDate = NULL, toDate = NULL,
  env_name = NULL, safedir = NULL, parse = TRUE, token = NULL) {

  search_premium("fullarchive", 
    q = q, 
    n = n,
    fromDate = fromDate,
    toDate = toDate,
    env_name = env_name,
    safedir = safedir,
    parse = parse,
    token = token
  )

}

#' @rdname search_fullarchive
#' @export
search_30day <- function(q, n = 100, fromDate = NULL, toDate = NULL,
                          env_name = NULL, safedir = NULL,
                          parse = TRUE,
                          token = NULL) {
  
  search_premium("30day", 
    q = q, 
    n = n,
    fromDate = fromDate,
    toDate = toDate,
    env_name = env_name,
    safedir = safedir,
    parse = parse,
    token = token
  )
}


search_premium <- function(product, q, n = 100, fromDate = NULL, toDate = NULL,
                          env_name = NULL, safedir = NULL,
                          parse = TRUE,
                          token = NULL) {
  
  if (is.null(env_name)) {
    stop("Must provide dev environment name")
  }
  if (!is.null(safedir)) {
    stop("`safedir` temporarily not supported")
  }
  
  params <- search_params(q, 
    n = n,
    fromDate = format_from_to_date(fromDate),
    toDate = format_from_to_date(toDate),
  )
  
  api <- paste0("/1.1/search/tweets/", product, "/", env_name)
  result <- TWIT_paginate_max_id(token, api, params,
    get_id = function(x) x$statuses$id_str,
    max_id = max_id,
    n = n,
    page_size = if (env_name == "sandbox") 100 else 500, 
    parse = parse,
    count_param = "maxResults"
  )

  if (parse) {
    result <- tweets_with_users(result)
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

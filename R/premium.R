

premium_api <- function(...) {
  dots <- c(...)
  if (length(dots) == 1) {
    dots <- dots[[1]]
  }
  path <- grep("30day|fullarchive", dots, value = TRUE)
  env_name <- grep("30day|fullarchive", dots, value = TRUE, invert = TRUE)
  list(path = path, env_name = env_name)
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


#' Search fullarchive (PREMIUM)
#'
#' Search Twitter's 'fullarchive' (PREMIUM) API
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
#' @param parse Logical indicating whether to convert data into data frame.
#' @param safedir Name of directory to which each response object should be
#'   saved. If the directory doesn't exist, it will be created. If NULL (the
#'   default) then a dir will be created in the current working directory. To
#'   override/deactivate safedir set this to FALSE.
#' @param token A token associated with a user-created APP (requires a developer
#'   account), which is converted to a bearer token in order to make premium
#'   API requests
#'
#' @section Developer Account:
#' Users must have an approved developer account and an active/labeled
#' environment to access Twitter's premium APIs. For more information, to check
#' your current Subscriptions and Dev Environments, or to apply for a developer
#' account visit \url{https://developer.twitter.com}.
#'
#' @section Search operators:
#' \emph{Note: Bolded operators ending with a colon should be immediately
#' followed by a word or quoted phrase (if appropriate)–e.g.,} \code{lang:en}
#'
#' @section Keyword:
#' \itemize{
#'   \item \strong{""}           ~~ match exact phrase
#'   \item \strong{#}               ~~ hashtag
#'   \item \strong{@@}               ~~ at mentions)
#'   \item \strong{url:}            ~~ found in URL
#'   \item \strong{lang:}           ~~ language of tweet
#' }
#'
#' @section Accounts of interest:
#' \itemize{
#'   \item \strong{from:}           ~~ authored by
#'   \item \strong{to:}             ~~ sent to
#'   \item \strong{retweets_of:}    ~~ retweet author
#' }
#'
#' @section Tweet attributes:
#' \itemize{
#'   \item \strong{is:retweet}      ~~ only retweets
#'   \item \strong{has:mentions}    ~~ uses mention(s)
#'   \item \strong{has:hashtags}    ~~ uses hashtags(s)
#'   \item \strong{has:media}       ~~ includes media(s)
#'   \item \strong{has:videos}      ~~ includes video(s)
#'   \item \strong{has:images}      ~~ includes image(s)
#'   \item \strong{has:links}       ~~ includes URL(s)
#'   \item \strong{is:verified}     ~~ from verified accounts
#' }
#'
#' @section Geospatial:
#' \itemize{
#'   \item \strong{bounding_box:[west_long south_lat east_long north_lat]} ~~ lat/long coordinates box
#'   \item \strong{point_radius:[lon lat radius]} ~~ center of search radius
#'   \item \strong{has:geo}           ~~ uses geotagging
#'   \item \strong{place:}            ~~ by place
#'   \item \strong{place_country:}    ~~ by country
#'   \item \strong{has:profile_geo}   ~~ geo associated with profile
#'   \item \strong{profile_country:}  ~~ country associated with profile
#'   \item \strong{profile_region:}   ~~ region associated with profile
#'   \item \strong{profile_locality:} ~~ locality associated with profile
#' }
#'
#' @return A tibble data frame of Twitter data
#' @examples
#'
#' \dontrun{
#' ## search fullarchive for up to 300 rstats tweets sent in Jan 2014
#' rt <- search_fullarchive("#rstats", n = 300, env_name = "research",
#'   fromDate = "201401010000", toDate = "201401312359")
#' }
#'
#' @export
search_fullarchive <- function(q, n = 100, fromDate = NULL, toDate = NULL,
  env_name = NULL, safedir = NULL, parse = TRUE, token = NULL) {
  token <- check_token(token)
  if (!length(get_app_secret(token))) {
    stop(paste0("This token does not have an app secret and therefore cannot ",
    "create a bearer token"), call. = FALSE)
  }
  token <- bearer_token(token)
  if (is.null(safedir) || isTRUE(safedir)) {
    safedir <- paste0("premium-", format(Sys.Date(), "%Y%m%d"))
  } else if (isFALSE(safedir)) {
    safedir <- NULL
  } else {
    safedir <- safedir
  }
  fromDate <- format_from_to_date(fromDate)
  toDate <- format_from_to_date(toDate)
  if (is.null(env_name)) {
    stop("Must provide dev environment name")
  }
  r <- search_tweets(q,
    fromDate = fromDate,
    toDate = toDate,
    premium = premium_api("fullarchive", env_name),
    parse = FALSE, n = n,
    safedir = safedir,
    token = token)
  if (parse) {
    np <- get_next_page(r)
    r <- tweets_with_users(r)
    attr(r, "next_page") <- np
  }
  r
}

#' Search last 30day (PREMIUM)
#'
#' Search Twitter's '30day' (PREMIUM) API
#'
#' @inheritParams search_fullarchive
#'
#' @section Developer Account:
#' Users must have an approved developer account and an active/labeled
#' environment to access Twitter's premium APIs. For more information, to check
#' your current Subscriptions and Dev Environments, or to apply for a developer
#' account visit \url{https://developer.twitter.com}.
#'
#' @section Search operators:
#' \emph{Note: Bolded operators ending with a colon should be immediately
#' followed by a word or quoted phrase (if appropriate)–e.g.,} \code{lang:en}
#'
#' @section Keyword:
#' \itemize{
#'   \item \strong{""}           ~~ match exact phrase
#'   \item \strong{#}               ~~ hashtag
#'   \item \strong{@@}               ~~ at mentions)
#'   \item \strong{url:}            ~~ found in URL
#'   \item \strong{lang:}           ~~ language of tweet
#' }
#'
#' @section Accounts of interest:
#' \itemize{
#'   \item \strong{from:}           ~~ authored by
#'   \item \strong{to:}             ~~ sent to
#'   \item \strong{retweets_of:}    ~~ retweet author
#' }
#'
#' @section Tweet attributes:
#' \itemize{
#'   \item \strong{is:retweet}      ~~ only retweets
#'   \item \strong{has:mentions}    ~~ uses mention(s)
#'   \item \strong{has:hashtags}    ~~ uses hashtags(s)
#'   \item \strong{has:media}       ~~ includes media(s)
#'   \item \strong{has:videos}      ~~ includes video(s)
#'   \item \strong{has:images}      ~~ includes image(s)
#'   \item \strong{has:links}       ~~ includes URL(s)
#'   \item \strong{is:verified}     ~~ from verified accounts
#' }
#'
#' @section Geospatial:
#' \itemize{
#'   \item \strong{bounding_box:[west_long south_lat east_long north_lat]} ~~ lat/long coordinates box
#'   \item \strong{point_radius:[lon lat radius]} ~~ center of search radius
#'   \item \strong{has:geo}           ~~ uses geotagging
#'   \item \strong{place:}            ~~ by place
#'   \item \strong{place_country:}    ~~ by country
#'   \item \strong{has:profile_geo}   ~~ geo associated with profile
#'   \item \strong{profile_country:}  ~~ country associated with profile
#'   \item \strong{profile_region:}   ~~ region associated with profile
#'   \item \strong{profile_locality:} ~~ locality associated with profile
#' }
#'
#' @return A tibble data frame of Twitter data
#' @examples
#'
#' \dontrun{
#' ## format datetime for one week ago
#' toDate <- format(Sys.time() - 60 * 60 * 24 * 7, "%Y%m%d%H%M")
#'
#' ## search 30day for up to 300 rstats tweets sent before the last week
#' rt <- search_30day("#rstats", n = 300,
#'   env_name = "research", toDate = toDate)
#' }
#'
#' @export
search_30day <- function(q, n = 100, fromDate = NULL, toDate = NULL,
                          env_name = NULL, safedir = NULL,
                          parse = TRUE,
                          token = NULL) {
  token <- check_token(token)
  if (!length(get_app_secret(token))) {
    stop(paste0("This token does not have an app secret and therefore cannot ",
      "create a bearer token"), call. = FALSE)
  }
  token <- bearer_token(token)
  if (is.null(safedir) || isTRUE(safedir)) {
    safedir <- paste0("premium-", format(Sys.Date(), "%Y%m%d"))
  } else if (isFALSE(safedir)) {
    safedir <- NULL
  } else {
    safedir <- safedir
  }
  fromDate <- format_from_to_date(fromDate)
  toDate <- format_from_to_date(toDate)
  if (is.null(env_name)) {
    stop("Must provide dev environment name")
  }
  r <- search_tweets(q,
    fromDate = fromDate,
    toDate = toDate,
    premium = premium_api("30day", env_name),
    parse = FALSE, n = n,
    safedir = safedir,
    token = token)
  if (parse) {
    np <- get_next_page(r)
    r <- tweets_with_users(r)
    attr(r, "next_page") <- np
  }
  r
}

#' GET trends/place
#'
#' @description Returns Twitter trends
#'
#' @param woeid Numeric, WOEID (Yahoo! Where On Earth ID) or
#'   character string of desired town or country. To browse all
#'   available trend places, see \code{\link{trends_available}}
#' @param ... For more info on all possible arguments see
#'   \code{\link{get_trends.default}}.
#' @examples
#' \dontrun{
#' # Retrieve available trends
#' trends <- available_trends()
#' trends
#'
#' # Store WOEID for Worldwide trends
#' worldwide <- subset(trends, name == "Worldwide")[["woeid"]]
#'
#' # Retrieve worldwide trends datadata
#' ww_trends <- get_trends(woeid = worldwide)
#'
#' # Preview trends data
#' ww_trends
#' }
#'
#' @return Trend data for a given location.
#' @family trends
#' @export
get_trends <- function(woeid = 1, ...) {
  UseMethod("get_trends")
}

#' get_trends.default
#'
#' Returns trend data by location
#'
#' @param woeid Numeric, WOEID (Yahoo! Where On Earth ID) or
#'   character string of desired town or country. To browse all
#'   available trend places, see \code{\link{trends_available}}
#' @param exclude Logical, indicating whether or not to exclude
#'   hashtags
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find instructions
#'   on how to create tokens and setup an environment variable in the
#'   tokens vignette (in r, send \code{?tokens} to console).
#' @param parse Logical, indicating whether or not to parse return
#'   trends data.
#' @return Tibble data frame of trends data.
#' @family trends
#' @export
get_trends.default <- function(woeid = 1,
                               exclude = FALSE,
                               token = NULL,
                               parse = TRUE) {

  stopifnot(is.atomic(woeid), length(woeid) == 1)

  woeid <- check_woeid(woeid)

  query <- "trends/place"

  token <- check_token(token, query)

  params <- list(
    id = woeid,
    exclude = exclude)

  url <- make_url(
    query = query,
    param = params)

  gt <- TWIT(get = TRUE, url, token)

  gt <- from_js(gt)

  if (parse) {
    gt <- parse_trends(gt)
  }
  gt
}

#' get_trends_closest
#'
#' @description Returns Twitter trends
#'
#' @param lat Numeric, Latitude in degrees
#' @param long Numeric, Longitude in degrees
#' @param exclude Logical, indicating whether or not to exclude
#'   hashtags
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find instructions
#'   on how to create tokens and setup an environment variable in the
#'   tokens vignette (in r, send \code{?tokens} to console).
#' @param parse Logical, indicating whether or not to parse return
#'   trends data.
#'
#' @examples
#' \dontrun{
#'
#' # Retrieve trends data for latitude, longitude near New York City
#' nyc_trends <- get_trends_closest(lat = 40.7, long=-74.0, token=twitter_token)
#'
#' # Preview trends data
#' nyc_trends
#' }
#'
#' @return Trend data closest to a given location.
#' @family trends
#' @export
get_trends_closest <- function(lat = NULL,
                               long = NULL,
                               exclude = FALSE,
                               token = NULL,
                               parse = TRUE) {
  query <- "trends/place"

  stopifnot(!is.null(lat), !is.null(long))
  token <- check_token(token, query)

  woeid <- trends_closest(lat, long, token)$woeid

  get_trends(woeid = woeid, exclude = exclude, token = token, parse = parse)
}

#' Get closest available location to given Lat, Long
#'
#' @description Uses the trends/closest API endpoint to get the
#'  the closest location to a given Latitude, Longitude pair. Returns
#'  a data frame describing the location.
#' @param lat Latitude in degrees.
#' @param long Longitude in degrees.
#' @param token OAuth token.
#' @examples
#' \dontrun{
#' # New York is ~ 40.7N, 74.0W
#' nyc_woeid <- trends_closest(40.7, -74.0, twitter_token)
#' }
trends_closest <- function(lat, long, token) {
  query <- "trends/closest"
  token <- check_token(token, query)
  url <- make_url(query = query,
                  param = list(lat=lat, long=long))
  trd <- TWIT(get = TRUE, url, token)
  from_js(trd)
}

parse_trends <- function(x) {
  trends <- data.frame(x$trends[[1]],
                       stringsAsFactors = FALSE)
  rows <- nrow(trends)
  names(trends)[names(trends) == "name"] <- "trend"
  trends <- cbind(
    trends,
    data.frame(
      as_of = format_trend_date(rep(x$as_of, rows)),
      created_at = format_trend_date(rep(x$created_at, rows)),
      place = rep(x$locations[[1]]$name, rows),
      woeid = rep(x$locations[[1]]$woeid, rows)),
    stringsAsFactors = FALSE
  )
  tibble::as_tibble(trends, validate = FALSE)
}


format_trend_date <- function(x, date = FALSE) {
  x <- as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ",
                  tz = Sys.timezone())
  if (date) {
    x <- as.Date(x)
  }
  x
}

#' trends_available
#'
#' @description Returns Twitter trends based on requested WOEID.
#'
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find instructions
#'   on how to create tokens and setup an environment variable in the
#'   tokens vignette (in r, send \code{?tokens} to console).
#' @param parse Logical, indicating whether to return parsed
#'   (data.frames) or nested list (fromJSON) object. By default,
#'   \code{parse = TRUE} saves users from the time
#'   [and frustrations] associated with disentangling the Twitter
#'   API return objects.
#'
#' @examples
#' \dontrun{
#' # Retrieve available trends
#' trends <- available_trends()
#' trends
#'
#' # Store WOEID for Worldwide trends
#' worldwide <- subset(trends, name == "Worldwide")[["woeid"]]
#'
#' # Retrieve worldwide trends datadata
#' ww_trends <- get_trends(woeid = Worldwide)
#'
#' # Preview Worldwide trends data
#' ww_trends
#' }
#'
#' @return Data frame with WOEIDs. WOEID is a Yahoo! Where On
#'   Earth ID.
#' @family trends
#' @export
trends_available <- function(token = NULL, parse = TRUE) {
    query <- "trends/available"
    token <- check_token(token, query)
    url <- make_url(query = query,
                    param = NULL)
    trd <- TWIT(get = TRUE, url, token)
    trd <- from_js(trd)
    if (parse) trd <- parse_trends_available(trd)
    trd
}

parse_trends_available <- function(x) {
    p <- cbind(data.frame(x[names(x) != "placeType"],
                          stringsAsFactors = FALSE),
               data.frame(x[["placeType"]],
                          stringsAsFactors = FALSE))
    names(p)[ncol(p)] <- "place_type"
    p
}


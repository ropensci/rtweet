#' Get Twitter trends data.
#'
#' @param woeid Numeric, WOEID (Yahoo! Where On Earth ID) or character
#'   string of desired town or country. Users may also supply latitude
#'   and longitude coordinates to fetch the closest available trends
#'   data given the provided location. Latitude/longitude coordinates
#'   should be provided as WOEID value consisting of 2 numeric values
#'   or via one latitude value and one longitude value (to the
#'   appropriately named parameters).  To browse all available trend
#'   places, see \code{\link{trends_available}}
#' @param lat Optional alternative to WOEID. Numeric, latitude in
#'   degrees.  If two coordinates are provided for WOEID, this
#'   function will coerce the first value to latitude.
#' @param lng Optional alternative to WOEID. Numeric, longitude in
#'   degrees.  If two coordinates are provided for WOEID, this
#'   function will coerce the second value to longitude.
#' @param exclude_hashtags Logical, indicating whether or not to
#'   exclude hashtags. Defaults to FALSE--meaning, hashtags are
#'   included in returned trends.
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find
#'   instructions on how to create tokens and setup an environment
#'   variable in the tokens vignette (in r, send \code{?tokens} to
#'   console).
#' @param parse Logical, indicating whether or not to parse return
#'   trends data. Defaults to true.
#' @examples
#'
#' \dontrun{
#'
#' ## Retrieve available trends
#' trends <- trends_available()
#' trends
#'
#' ## Store WOEID for Worldwide trends
#' worldwide <- trends$woeid[grep("world", trends$name, ignore.case = TRUE)[1]]
#'
#' ## Retrieve worldwide trends datadata
#' ww_trends <- get_trends(worldwide)
#'
#' ## Preview trends data
#' ww_trends
#'
#' ## Retrieve trends data using latitude, longitude near New York City
#' nyc_trends <- get_trends_closest(lat = 40.7, lng = -74.0)
#'
#' ## should be same result if lat/long supplied as first argument
#' nyc_trends <- get_trends_closest(c(40.7, -74.0))
#'
#' ## Preview trends data
#' nyc_trends
#'
#' ## Provide a city or location name using a regular expression string to
#' ## have the function internals do the WOEID lookup/matching for you
#' (luk <- get_trends("london"))
#'
#' }
#'
#' @return Tibble data frame of trends data for a given geographical area.
#' @family trends
#' @export
get_trends <- function(woeid = 1,
                       lat = NULL,
                       lng = NULL,
                       exclude_hashtags = FALSE,
                       token = NULL,
                       parse = TRUE) {
  args <- list(
    woeid = woeid,
    lat = lat,
    lng = lng,
    exclude = exclude_hashtags,
    token = token,
    parse = parse)
  do.call("get_trends_", args)
}

is_latlng <- function(x) {
  if (!is.numeric(x) || length(x) != 2L) return(FALSE)
  x[1] <= 90 && x[1] >= -90 && x[2] <= 180 && x[2] >= -180
}

get_trends_ <- function(woeid = 1,
                        lat = NULL,
                        lng = NULL,
                        exclude = FALSE,
                        token = NULL,
                        parse = TRUE) {
  if (inherits(woeid, "coords")) {
    lat <- woeid$point[1]
    lng <- woeid$point[2]
  } else if (is_latlng(woeid)) {
    lat <- woeid[1]
    lng <- woeid[2]
  }
  if (!is.null(lat) && !is.null(lng)) {
    stopifnot(maybe_n(lat), maybe_n(lng))
    woeid <- trends_closest(lat, lng, token = token)
    woeid <- as.character(woeid$woeid[1])
    if (length(woeid) == 0L || is.na(woeid)) {
      stop("could not find woe id for provided lat/lng coordinates", call. = FALSE)
    }
  } else {
    stopifnot(is.atomic(woeid), length(woeid) == 1)
    if (!is_n(woeid)) {
      trends <- trends_available(token = token)
      woeid <- trends$woeid[grep(woeid, trends$name, ignore.case = TRUE)[1]]
      if (length(woeid) == 0L || is.na(woeid)) {
        stop("Could not find trend data for that location", call. = FALSE)
      }
    }
    woeid <- check_woeid(woeid)
  }
  query <- "trends/place"
  token <- check_token(token, query)
  if (exclude) {
    exclude <- "hashtags"
  } else {
    exclude <- NULL
  }
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


trends_closest <- function(lat, long, token = NULL) {
  query <- "trends/closest"
  token <- check_token(token, query)
  url <- make_url(query = query,
                  param = list(lat = lat, long = long))
  trd <- TWIT(get = TRUE, url, token)
  from_js(trd)
}

parse_trends <- function(x) {
  if (!is.recursive(x) || !has_name_(x, "trends")) return(data.frame())
  locations <- x$locations[[1]]
  place <- var_or_na(locations$name)
  woeid <- var_or_na(locations$woeid)
  as_of <- format_trend_date(x$as_of)
  created_at <- format_trend_date(x$created_at)
  x <- x$trends[[1]]
  if (!is.data.frame(x)) {
    x <- as.data.frame(x, stringsAsFactors = FALSE)
  }
  names(x)[names(x) == "name"] <- "trend"
  x$place <- place
  x$woeid <- woeid
  x$as_of <- as_of
  x$created_at <- created_at
  tibble::as_tibble(x, validate = FALSE)
}

var_or_na <- function(x) {
  if (length(x) == 0L) return(NA_character_)
  x
}

format_trend_date <- function(x) {
  if (length(x) == 0L) return(as.POSIXct(NA_character_, tz = ""))
  if (all(is.na(x))) return(as.POSIXct(NA_character_, tz = ""))
  x <- as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "")
  if (all(is.na(x))) return(as.POSIXct(NA_character_, tz = ""))
  x
}

#' Available Twitter trends along with associated WOEID.
#'
#' @param token OAuth token. By default \code{token = NULL} fetches a
#'   non-exhausted token from an environment variable. Find instructions
#'   on how to create tokens and setup an environment variable in the
#'   tokens vignette (in r, send \code{?tokens} to console).
#' @param parse Logical, indicating whether to return parsed
#'   (data.frames) or nested list object. By default,
#'   \code{parse = TRUE} saves users from the time
#'   [and frustrations] associated with disentangling the Twitter
#'   API return objects.
#'
#' @examples
#' \dontrun{
#' ## Retrieve available trends
#' trends <- trends_available()
#' trends
#'
#' }
#'
#' @return Data frame with WOEID column. WOEID is a Yahoo! Where On
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
                        stringsAsFactors = FALSE),
             stringsAsFactors = FALSE)
  names(p)[ncol(p)] <- "place_type"
  tibble::as_tibble(p, validate = FALSE)
}


find_woeid <- function(x) {
  if (length(x) == 0L) {
    warning(paste0(
      "unable to find matching location.",
      "Using WOEID for Worldwide trends instead."))
    x <- "1"
  } else if (length(match_woeid(x)) > 0L) {
    x <- match_woeid(x)
  } else {
    warning(paste0(
      "unable to find matching location.",
      "Using WOEID for Worldwide trends instead."))
    x <- "1"
  }
  if (length(x) == 0L) {
    warning(paste0(
      "unable to find matching location.",
      "Using WOEID for Worldwide trends instead."))
    x <- "1"
  }
  if (length(x) > 1L) {
    x <- x[1L]
  }
  x
}

check_woeid <- function(x) {
  if (is_n(x)) return(as.character(x))
  x <- find_woeid(x)
  as.character(x)
}

is_zero <- function(x) isTRUE(identical(length(x), 0L))

match_woeid <- function(x) {
  if (tolower(x) %in% c("world", "worldwide",
                        "world wide", "all")) {
    return("1")
  } else if (tolower(x) %in% c("us", "u.s.", "u s", "usa", "unitedstates")) {
    return("23424977")
  } else {
    places <- sysdat$woeid[["name"]]
    woeids <- as.character(sysdat$woeid[["woeid"]])
    woeids[tolower(places) == tolower(x)]
  }
}

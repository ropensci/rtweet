#' Get Twitter trends data.
#'
#' `r lifecycle::badge("deprecated")`
#' @inheritParams lookup_users
#' @param woeid Numeric, WOEID (Yahoo! Where On Earth ID) or character
#'   string of desired town or country. Users may also supply latitude
#'   and longitude coordinates to fetch the closest available trends
#'   data given the provided location. Latitude/longitude coordinates
#'   should be provided as WOEID value consisting of 2 numeric values
#'   or via one latitude value and one longitude value (to the
#'   appropriately named parameters).  To browse all available trend
#'   places, see [trends_available()]
#' @param lat Optional alternative to WOEID. Numeric, latitude in
#'   degrees.  If two coordinates are provided for WOEID, this
#'   function will coerce the first value to latitude.
#' @param lng Optional alternative to WOEID. Numeric, longitude in
#'   degrees.  If two coordinates are provided for WOEID, this
#'   function will coerce the second value to longitude.
#' @param exclude_hashtags Logical, indicating whether or not to
#'   exclude hashtags. Defaults to FALSE--meaning, hashtags are
#'   included in returned trends.
#' @inheritParams stream
#' @return Tibble data frame of trends data for a given geographical area.
#' @family trends
#' @seealso [`rtweet-deprecated`]
#' @export
get_trends <- function(woeid = 1,
                       lat = NULL,
                       lng = NULL,
                       exclude_hashtags = FALSE,
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
    stopifnot(is.atomic(woeid) && !is.null(woeid), length(woeid) == 1)
    if (!is_n(woeid)) {
      trends <- trends_available(token = token)
      woeid <- trends$woeid[grep(woeid, trends$name, ignore.case = TRUE)[1]]
      if (length(woeid) == 0L || is.na(woeid)) {
        stop("Could not find trend data for that location", call. = FALSE)
      }
    }
    woeid <- check_woeid(woeid)
  }

  params <- list(
    id = woeid,
    exclude = if (exclude_hashtags) "hashtags"
  )

  gt <- TWIT_get(token, "/1.1/trends/place", params)
  if (parse) {
    gt <- parse_trends(gt)
  }
  gt
}


is_latlng <- function(x) {
  if (!is.numeric(x) || length(x) != 2L) return(FALSE)
  x[1] <= 90 && x[1] >= -90 && x[2] <= 180 && x[2] >= -180
}

trends_closest <- function(lat, long, token = NULL) {
  params <- list(lat = lat, long = long)
  TWIT_get(token, "/1.1/trends/closest", params)
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
  tibble::as_tibble(x)
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
#' `r lifecycle::badge("deprecated")`
#' @inheritParams lookup_users
#'
#' @return Data frame with WOEID column. WOEID is a Yahoo! Where On
#'   Earth ID.
#' @family trends
#' @seealso [`rtweet-deprecated`]
#' @export
#' @references <https://developer.twitter.com/en/docs/twitter-api/v1/trends/locations-with-trending-topics/api-reference/get-trends-available>
trends_available <- function(token = NULL, parse = TRUE) {
  trd <- TWIT_get(token, "/1.1/trends/available")
  if (parse) {
    trd <- parse_trends_available(trd)
  }
  trd
}

parse_trends_available <- function(x) {
  p <- cbind(data.frame(x[names(x) != "placeType"],
                        stringsAsFactors = FALSE),
             data.frame(x[["placeType"]],
                        stringsAsFactors = FALSE),
             stringsAsFactors = FALSE)
  names(p)[ncol(p)] <- "place_type"
  tibble::as_tibble(p)
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

match_woeid <- function(x) {
  if (tolower(x) %in% c("world", "worldwide",
                        "world wide", "all")) {
    return("1")
  } else if (tolower(x) %in% c("us", "u.s.", "u s", "usa", "unitedstates")) {
    return("23424977")
  } else {
    # This function relies on data from WOEID that was last updated in 2012!
    places <- sysdat$woeid[["name"]]
    woeids <- as.character(sysdat$woeid[["woeid"]])
    woeids[tolower(places) == tolower(x)]
  }
}

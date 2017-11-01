##----------------------------------------------------------------------------##
##                          COMPUTE LAT/LNG VARIABLES                         ##
##----------------------------------------------------------------------------##

#' Adds single-point latitude and longitude variables to tweets data.
#'
#' Appends parsed tweets data with latitude and longitude variables
#' using all available geolocation information.
#'
#' @param x Parsed tweets data as returned by various rtweet
#'   functions. This should be a data frame with variables such as
#'   "bbox_coords", "coords_coords", and "geo_coords" (among
#'   other non-geolocation Twitter variables).
#' @param coords Names of variables containing latitude and longitude
#'   coordinates.  Priority is given to bounding box coordinates (each
#'   obs consists of eight entries) followed by the supplied order of
#'   variable names. Defaults to "bbox_coords",
#'   "coords_coords", and "geo_coords") (which are the default column
#'   names of data returned by most status-oriented rtweet functions).
#' @details On occasion values may appear to be outliers given a
#'   previously used query filter (e.g., when searching for tweets
#'   sent from the continental US).  This is typically because those
#'   tweets returned a large bounding box that overlapped with the
#'   area of interest. This function converts boxes into their
#'   geographical midpoints, which works well in the vast majority of
#'   cases, but sometimes includes an otherwise puzzling result.
#' @return Returns updated data object with full information latitude
#'   and longitude vars.
#' @family geo
#' @examples
#'
#' \dontrun{
#'
#' ## stream tweets sent from the US
#' rt <- stream_tweets(lookup_coords("usa"), timeout = 10)
#'
#' ## use lat_lng to recover full information geolocation data
#' rtll <- lat_lng(rt)
#'
#' ## plot points
#' with(rtll, plot(lng, lat))
#'
#' }
#'
#' @export
lat_lng <- function(x, coords = c("bbox_coords", "coords_coords", "geo_coords")) {
  stopifnot(is.data.frame(x))
  geodat <- lapply(coords, if_has_else_na, x = x, na_ = NA_real_)
  if (any(grepl("box", coords))) {
    bbox <- geodat[[grep("box", coords)[1]]]
    x <- cbind(x, lnglat(bbox))
  } else {
    x$lng <- NA_real_
    x$lat <- NA_real_
  }
  if (!any(is.na(unlist(bbox)))) {
    return(tibble::as_tibble(x, validate = FALSE))
  }
  coords <- grep("box", coords, invert = TRUE)
  if (length(coords) == 0L) {
    return(tibble::as_tibble(x, validate = FALSE))
  }
  for (i in seq_along(coords)) {
    x <- update_if_na(x, coords[i])
  }
  tibble::as_tibble(x, validate = FALSE)
}

lnglat_ <- function(x) {
  stopifnot(is.atomic(x))
  if (all(is.na(x)) || length(x) < 2L) {
    return(c(NA_real_, NA_real_))
  }
  ln <- length(x)
  mid <- ln %/% 2
  lng <- mean(x[seq(1L, mid, 1L)], na.rm = TRUE)
  lat <- mean(x[seq(ln, mid + 1L, -1L)], na.rm = TRUE)
  c(lng, lat)
}

lnglat <- function(x) {
  x <- Map(lnglat_, x)
  lng <- unlist(lapply(x, "[[", 1L), use.names = FALSE)
  lat <- unlist(lapply(x, "[[", 2L), use.names = FALSE)
  cbind(lng, lat)
}

area <- function(x) {
  f <- function(x) abs(x[1] - x[3]) * abs(x[5] - x[7])
  vapply(x, f, numeric(1))
}

if_has_else_na <- function(var, x, na_ = NA) {
  if (has_name_(x, var)) {
    x[[var]]
  } else {
    rep(na_, nrow(x))
  }
}

update_if_na <- function(x, coords) {
  ## if each element doesn't contain two numerics, return x
  if (!all(lengths(coords) == 2L)) {
    return(x)
  }
  ## if coords data contains any non-missing data
  if (any(!is.na(unlist(coords)))) {
    ## first data point assumed to be longitude
    coords_lng <- unlist(lapply(coords, "[[", 1L), use.names = FALSE)
    x$lng[is.na(x$lng)] <- coords_lng[is.na(x$lng)]
    ## second data point assumed to be latitude
    coords_lat <- unlist(lapply(coords, "[[", 2L), use.names = FALSE)
    x$lat[is.na(x$lat)] <- coords_lat[is.na(x$lat)]
  }
  ## return data object
  x
}

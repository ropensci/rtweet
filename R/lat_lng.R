##----------------------------------------------------------------------------##
##                          COMPUTE LAT/LNG VARIABLES                         ##
##----------------------------------------------------------------------------##

#' Adds single-point latitude and longitude variables to tweets data.
#'
#' Appends parsed Twitter data with latitude and longitude variables
#' using all available geolocation information.
#'
#' @param x Parsed Twitter data as returned by various rtweet
#'   functions. This should be a data frame with variables such as
#'   "bbox_coords", "coords_coords", and "geo_coords" (among
#'   other non-geolocation Twitter variables).
#' @param coords Names of variables containing latitude and longitude
#'   coordinates.  Priority is given to bounding box coordinates (each
#'   obs consists of eight entries) followed by the supplied order of
#'   variable names. Defaults to "bbox_coords",
#'   "coords_coords", and "geo_coords") (which are the default column
#'   names of data returned by most status-oriented rtweet functions).
#' @param prefs Preference of coordinates to use as default, must be in `coords`.
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
# Used by Twitmo
#' @export
lat_lng <- function(x, coords = c("coords_coords", "bbox_coords", "geo_coords"), prefs = "bbox_coords") {
  stopifnot(is.data.frame(x))
  coords <- match.arg(coords, several.ok = TRUE)
  prefs <- match.arg(prefs, choices = c("coords_coords", "bbox_coords", "geo_coords"))
  prefs <- prefs[prefs %in% coords]
  if (length(prefs) == 0) {
    abort("`prefs` is not included in `coords`")
  } else if (length(prefs) > 1) {
    abort("`prefs` must be of length 1")
  }

  lat_lang <- function(x) {

    if (is.null(x)) {
      return(list(long = NA, lat = NA))
    }

    # Protect against AsIs olClass data.frames in a list in x
    if (is.list(x) && !is.data.frame(x) && length(x) == 1) {
      x <- as.data.frame(x[[1]])
    }

    list(long = mean(range(x$long)),
         lat = mean(range(x$lat)))
  }
  l <- lapply(x$place, function(x) {
    if (length(x) == 1 && is.na(x)) {
      empty_coord <- list(long = NA, lat = NA)
      df <- data.frame(bbox_coords = I(list(empty_coord)),
                       geo_coords = I(list(empty_coord)),
                       coords_coords = I(list(empty_coord)))
      return(df)
    }

    data.frame(bbox_coords = I(list(lat_lang(x$place$bounding_box[[1]]))),
               geo_coords = I(list(lat_lang(x$geo))),
               coords_coords = I(list(lat_lang(x$coordinates))))
  })
  ll <- do.call(rbind, l)[, coords, drop = FALSE]

  lat <- vapply(ll, extract_coord, numeric(nrow(ll)), coord = "lat")
  if (!is.matrix(lat)) {
    lat <- list2DF(as.list(lat))
  }
  lat <- lat[, prefs]
  long <- vapply(ll, extract_coord, numeric(nrow(ll)), coord = "long")
  if (!is.matrix(long)) {
    long <- list2DF(as.list(long))
    }
  long <- long[, prefs]

  cbind(x, lat = lat, lng = long, ll)

}


extract_coord <- function(x, coord) {
  if (length(x) == 1) {
    return(x[[1]][[coord]])
  }

  vapply(x, function(y){
    y[[coord]]
  }, numeric(1L))
}

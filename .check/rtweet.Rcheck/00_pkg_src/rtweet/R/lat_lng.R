##----------------------------------------------------------------------------##
##                          COMPUTE LAT/LNG VARIABLES                         ##
##----------------------------------------------------------------------------##

#' lat_long
#'
#' Computes lat and lng variables and adds to data frame.
#'
#' @param x Data frame returned by rtweet function.
#' @param coords Names of variables containing latitude and longitude coordinates.
#'   Priority is given to bounding box coordinates (each obs consists of eight
#'   entries) followed by the supplied order of variable names. Defaults to
#'   c("bbox_coords", "coords_coords", "geo_coords").
#' @return Returns updated data object with full information lng and lat vars.
#' @export
lat_long <- function(x, coords = c("bbox_coords", "coords_coords", "geo_coords")) {
  geodat <- lapply(coords, if_has_else_na, x = x, na_ = NA_real_)
  if (any(grepl("box", coords))) {
    bbox <- geodat[[grep("box", coords)[1]]]
    x <- cbind(x, lnglat(bbox))
  } else {
    x$lng <- NA_real_
    x$lat <- NA_real_
  }
  if (!any(is.na(unlist(bbox)))) {
    return(x)
  }
  coords <- grep("box", coords, invert = TRUE)
  if (length(coords) == 0L) {
    return(x)
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
  if (var %in% names(x)) {
    x[[var]]
  } else {
    rep(na_, NROW(x))
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



#' Get coordinates of specified location.
#'
#' Convenience function for looking up latitude/longitude coordinate
#' information for a given location. Returns data as a special
#' "coords" object, which is specifically designed to interact
#' smoothly with other relevant package functions.
#'
#' @param address Desired location typically in the form of place
#'   name, subregion, e.g., address = "lawrence, KS". Also accepts the
#'   name of countries, e.g., address = "usa", address = "brazil" or
#'   states, e.g., address = "missouri" or cities, e.g., address =
#'   "chicago". In most cases using only address should be sufficient.
#' @param components Unit of analysis for address e.g., components =
#'   "country:US". Potential components include postal_code, country,
#'   administrative_area, locality, route.
#' @param ... Additional arguments passed as parameters in the HTTP
#'   request
#' @return Object of class coords.
#' @examples
#'
#' \dontrun{
#'
#' ## get coordinates associated with the following addresses/components
#' sf <- lookup_coords("san francisco, CA", "country:US")
#' usa <- lookup_coords("usa")
#' lnd <- lookup_coords("london")
#' bz <- lookup_coords("brazil")
#'
#' ## pass a returned coords object to search_tweets
#' bztw <- search_tweets(geocode = bz)
#'
#' ## or stream tweets
#' ustw <- stream_tweets(usa, timeout = 10)
#'
#' }
#'
#' @importFrom jsonlite fromJSON
#' @family geo
#' @export
lookup_coords <- function(address, components = NULL, ...) {
  if (missing(address)) stop("must supply address", call. = FALSE)
  stopifnot(is.atomic(address), is.atomic(components))
  place <- address
  if (grepl("^us$|^usa$|^united states$|^u\\.s",
            address, ignore.case = TRUE)) {
    boxp <- c(
      sw.lng = -124.848974,
      sw.lat = 24.396308,
      ne.lng = -66.885444,
      ne.lat = 49.384358
    )
    point <- c(
      lat = 36.89,
      lng = -95.867
    )
  } else if (grepl("^world$|^all$|^globe$|^earth$",
            address, ignore.case = TRUE)) {
    boxp <- c(
      sw.lng = -180,
      sw.lat = -90,
      ne.lng = 180,
      ne.lat = 90
    )
    point <- c(
      lat = 0,
      lng = 0
    )
  } else {
    ## encode address
    address <- gsub(" ", "+", gsub(",", "", address))
    ## compose query
    params <- list(address = address,
                   components = components,
                   ...)
    params <- params[!vapply(params, is.null, logical(1))]
    params <- paste0(
      mapply(function(x, y) paste0(x, "=", y),
             names(params), params),
      collapse = "&")
    ## build URL
    geourl <- paste0("https://maps.googleapis.com/maps/api/geocode/json?",
                     params)
    ## read and convert to list obj
    r <- jsonlite::fromJSON(geourl)
    ## extract and name box and point data frames
    boxp <- c(
      sw.lng = r$results$geometry$bounds$southwest$lng,
      sw.lat = r$results$geometry$bounds$southwest$lat,
      ne.lng = r$results$geometry$bounds$northeast$lng,
      ne.lat = r$results$geometry$bounds$northeast$lat
    )
    point <- c(
      lat = r$results$geometry$location$lat,
      lng = r$results$geometry$location$lng
    )
  }
  as.coords(place = place, box = boxp, point = point)
}


mean.dbls <- function(x) mean(as.double(x, na.rm = TRUE))


mutate.coords <- function(x) {
  if (is.data.frame(x)) {
    if ("place.bounding_box.coordinates" %in% names(x)) {
      coordinates <- x[["place.bounding_box.coordinates"]]
    } else if ("bounding_box_coordinates" %in% names(x)) {
      coordinates <- x[["bounding_box_coordinates"]]
    } else if ("coordinates" %in% names(x)) {
      coordinates <- x[["coordinates"]]
    }
  } else {
    coordinates <- x
  }

  if (is.character(coordinates)) {
    coordinates <- gsub(
      ",", " ", coordinates
    )
    coordinates <- strsplit(
      coordinates, " "
    )
  }

  if (is.list(coordinates)) {
    lns <- lengths(coordinates)
    if (all(lns < 3)) {
      coordinates <- do.call(
        "rbind",
        lapply(coordinates, function(x) matrix(x, 1, 2))
      )
    } else if (all(lns < 9)) {
      coordinates <- do.call(
        "rbind",
        lapply(coordinates, function(x) matrix(x, 1, 8))
      )
    }
  }

  if (any(is.data.frame(coordinates),
      is.matrix(coordinates))) {
    coordinates <- apply(coordinates, 2, as.double)
    if (ncol(coordinates) == 8) {
      coordinates <- cbind(
      rowMeans(coordinates[, 1:4], na.rm = TRUE),
      rowMeans(coordinates[, 5:8], na.rm = TRUE)
      )
    }
  }

  if (!any(is.data.frame(coordinates),
          is.matrix(coordinates),
          isTRUE(ncol(coordinates) == 2))) {
    lat <- rep(NA, NROW(x))
    lng <- rep(NA, NROW(x))
  } else {
    lat <- coordinates[, 2]
    lng <- coordinates[, 1]
  }
  latlng <- cbind(x, lat, lng)
  tibble::as_tibble(latlng, validate = FALSE)
}



as.coords <- function(place, box, point) {
  coords <- list(place = place, box = box, point =  point)
  class(coords) <- c("coords", "list")
  coords
}


max_coords <- function(x) {
  ypt <- apply(x@point, 1, function(.) all(is.na(.)))
  lng <- x@point[, 1]
  lat <- x@point[, 2]
  b <- x@box[ypt, ]
  lng[ypt] <- rowMeans(b[, 1:4], na.rm = TRUE)
  lat[ypt] <- rowMeans(b[, 5:8], na.rm = TRUE)
  lng[is.nan(lng)] <- NA_real_
  lat[is.nan(lat)] <- NA_real_
  cbind(
    lng = lng,
    lat = lat
  )
}

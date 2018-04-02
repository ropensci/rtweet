
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





as.coords <- function(place, box, point) {
  coords <- list(place = place, box = box, point =  point)
  class(coords) <- c("coords", "list")
  coords
}


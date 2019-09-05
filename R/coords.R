
#' Get coordinates of specified location.
#'
#' Convenience function for looking up latitude/longitude coordinate
#' information for a given location. Returns data as a special
#' "coords" object, which is specifically designed to interact
#' smoothly with other relevant package functions. NOTE: USE OF THIS FUNCTION
#' REQUIRES A VALID GOOGLE MAPS API KEY.
#'
#' @param address Desired location typically in the form of place
#'   name, subregion, e.g., address = "lawrence, KS". Also accepts the
#'   name of countries, e.g., address = "usa", address = "brazil" or
#'   states, e.g., address = "missouri" or cities, e.g., address =
#'   "chicago". In most cases using only address should be sufficient.
#' @param components Unit of analysis for address e.g., components =
#'   "country:US". Potential components include postal_code, country,
#'   administrative_area, locality, route.
#' @param apikey A valid Google Maps API key. If NULL, `lookup_coords()` will
#'   look for a relevant API key stored as an environment variable (e.g.,
#'   `GOOGLE_MAPS_KEY`).
#' @details Since Google Maps implemented stricter API requirements, sending 
#'   requests to Google's API isn't very convenient. To enable basic uses
#'   without requiring a Google Maps API key, a number of the major cities
#'   throughout the word and the following two larger locations are 
#'   baked into this function: 'world' and 'usa'. If 'world' is supplied then
#'   a bounding box of maximum latitutde/longitude values, i.e.,
#'   \code{c(-180, -90, 180, 90)}, and a center point \code{c(0, 0)} are 
#'   returned. If 'usa' is supplied then estimates of the United States' 
#'   bounding box and mid-point are returned. To specify a city, provide the
#'   city name followed by a space and then the US state abbreviation or 
#'   country name. To see a list of all included cities, enter
#'   \code{rtweet:::citycoords} in the R console to see coordinates data.
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
lookup_coords <- function(address, components = NULL, apikey = NULL, ...) {
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
  } else if (gsub("\\,\\s?", " ", tolower(address)) %in% c(citycoords$city, 
    sub("can$", "canada", citycoords$city), 
    sub("usa$", "us", citycoords$city))) {
    address <- gsub("\\,\\s?", " ", tolower(address))
    address <- sub("canada", "can", address)
    address <- sub("usa$", "us", address)
    i <- match(address, citycoords$city)
    point <- c(
      lat = citycoords$lat[i],
      lng = citycoords$lng[i])
    boxp <- c(
      sw.lng = point[2] - .05,
      sw.lat = point[1] - .05,
      ne.lng = point[2] + .05,
      ne.lat = point[1] + .05
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
    if (is.null(apikey)) {
      apikey <- find_google_geocode_key()
    }
    geourl <- paste0("https://maps.googleapis.com/maps/api/geocode/json?key=",
      apikey, "&", params)
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

find_google_geocode_key <- function() {
  evs <- names(Sys.getenv())
  if (any(grepl("GOOGLE.*MAP", evs, ignore.case = TRUE))) {
    p <- grep("GOOGLE.*MAP", evs, ignore.case = TRUE)
    p <- p[length(p)]
    key <- Sys.getenv(evs[p])
  } else if (any(grepl("GOOGLE.*KEY", evs, ignore.case = TRUE))) {
    p <- grep("GOOGLE.*KEY", evs, ignore.case = TRUE)
    p <- p[length(p)]
    key <- Sys.getenv(evs[p])
  } else if (interactive()) {
    yn <- menuline("lookup_users() requires a Google Maps API key (for instructions on how to acquire one, see: https://developers.google.com/maps/documentation/javascript/tutorial), Do you have a Google Maps API key you'd like to use?", c("Yes", "No"))
    if (yn == 2) {
      stop("sorry, lookup_users() requires a Google Maps API key")
    }
    key <- readline("Please enter your Google Maps API key:")
    key <- gsub("\\s+|\"|'", "", key)
    set_renv(GOOGLE_MAPS_KEY = key)
  } else {
    key <- ""
  }
  ## if invalid key
  if (identical(key, "")) {
    msg <- paste0("Invalid key. `lookup_users()` expects a non-empty string stored as ",
      "`GOOGLE_MAPS_KEY` in the \".Renviron\" file or passed via `apikey`, e.g., ",
      "`lookup_coords(\"London, UK\", apikey = \"MWIFdTiGjPqy-nPknKmvLLeOInVzETZVlRg_2mg\")`",
      ". For instructions on obtaining a Google Maps API key, see: ",
      "https://developers.google.com/maps/documentation/javascript/tutorial")
    stop(msg)
  }
  key
}

menuline <- function(q, a) {
  message(q)
  utils::menu(a)
}


as.coords <- function(place, box, point) {
  coords <- list(place = place, box = box, point =  point)
  class(coords) <- c("coords", "list")
  coords
}


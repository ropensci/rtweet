#' mutate_coords
#'
#' Initializes rt plotting sequence
#'
#' @param data Data frame generated via rtweet function.
#' @aliases mutate_coords
#' @export
mutate_coords <- function(data) {
    mutate.coords(data)
}

na_omit <- function(x) {
    x[!is.na(x)]
}

#' lookup_coords
#'
#' Returns coordinates using google geocode api
#'
#' @param address Desired location typically in the form of placename,
#'   subregion, e.g., \code{"lawrence, KS"}. Also accepts the name of
#'   countries, e.g., \code{"usa"}, \code{"brazil"} or states, e.g.,
#'   \code{"missouri"} or cities, e.g., \code{"chicago"}. In most
#'   cases using only address should be suffice.
#' @param components Unit of analysis for address e.g.,
#'    \code{"country:US"}. Potential components include postal_code,
#'    country, administrative_area, locality, route.
#' @param \dots Additional args passed along to params portion of
#'    http request
#' @return Object of class coords.
#' @examples
#' \dontrun{
#' sf <- lookup_coords("san francisco, CA", "country:US")
#' usa <- lookup_coords("usa")
#' lnd <- lookup_coords("london", box = FALSE)
#' bz <- lookup_coords("brazil")
#'
#' search_tweets(geocode = bz)
#' }
#' @importFrom jsonlite fromJSON
#' @export
lookup_coords <- function(address, components = NULL, ...) {
    if (missing(address)) stop("must supply address", call. = FALSE)
    stopifnot(is.atomic(address), is.atomic(components))
    place <- address
    if (grepl("^us$|^usa$|^united states$|^u\\.s", address, ignore.case = TRUE)) {
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
    } else {
      ## encode address
      address <- gsub(" ", "+", gsub(",", "", address))
      ## compose query
      params <- list(address = address,
                     components = components, ...)
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
    as_coords(place = place, box = boxp, point = point)
}

mean.dbls <- function(x) mean(as.double(x, na.rm = TRUE))

mutate.coords <- function(x) {
    if ("place.bounding_box.coordinates" %in% names(x)) {
        x[["place.bounding_box.coordinates"]] <- gsub(
            ",", " ", x[["place.bounding_box.coordinates"]]
        )
        coordinates <- strsplit(
            x[["place.bounding_box.coordinates"]], " ")
        x[["long"]] <- vapply(coordinates, function(x)
            mean.dbls(x[1:4]), double(1))
        x[["lat"]] <- vapply(coordinates, function(x)
            mean.dbls(x[5:8]), double(1))
    } else if ("bounding_box_coordinates" %in% names(x)) {
        x[["bounding_box_coordinates"]] <- gsub(
            ",", " ", x[["bounding_box_coordinates"]]
        )
        coordinates <- strsplit(
            x[["bounding_box_coordinates"]], " ")
        x[["long"]] <- vapply(coordinates, function(x)
            mean.dbls(x[1:4]), double(1))
        x[["lat"]] <- vapply(coordinates, function(x)
            mean.dbls(x[5:8]), double(1))
    } else if ("coordinates" %in% names(x)) {
        coordinates <- x[["coordinates"]]
        if (is.list(coordinates)) {
            coordinates <- coordinates %>%
                unlist() %>%
                as.double() %>%
                matrix(ncol = 8, byrow = TRUE)
        } else {
            coordinates <- gsub(
                ",", " ", coordinates
            )
            coordinates <- strsplit(
                coordinates, " ")
            x[["long"]] <- vapply(coordinates, function(x)
                mean.dbls(x[2]), double(1))
            x[["lat"]] <- vapply(coordinates, function(x)
                mean.dbls(x[1]), double(1))
        }
        x[["long"]] <- apply(
            coordinates, 1, function(x)
                mean.dbls(x[1:4]))
        x[["lat"]] <- apply(
            coordinates, 1, function(x)
                mean.dbls(x[5:8]))
    } else {
        stop("coordinates variable not found")
    }
    invisible(x)
}

#' coords class
#'
#' @name coords
NULL

#' @importFrom methods new
as_coords <- setClass(
  "coords", slots = c(place = "character", box = "numeric", point = "numeric"),
  prototype = list(place = character(),
                   box = numeric(4),
                   point = numeric(2)))

setMethod("show", "coords", function(object) print(object))

#' print coords
#'
#' @param x Object of class coords.
print.coords <- function(x) {
  message("An object of class \"coords\"")
  message("\nPlace: \"", x@place, "\"")
  message("\nBounding box:")
  print(x@box)
  message("\nPoint:")
  print(x@point)
}

setMethod("as.numeric", "coords", function(x) x@box)
setMethod("range", "coords", function(x) x@box)
setMethod("mean", "coords", function(x) x@point)
setMethod("unlist", "coords", function(x) x@box)
setMethod("$", "coords", function(x, name) slot(x, name))



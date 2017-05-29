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



#' lookup_coords
#'
#' Returns lat long coordinates using google geocode api
#'
#' @param address Desired location, e.g., \code{"lawrence, KS"}
#' @param components Unit of analysis for address e.g.,
#'    \code{"country:US"}. Potential components include postal_code,
#'    country, administrative_area, locality, route.
#' @param box Logical indicating whether to return a bounding box of
#'   coordinates (long1, lat1, long2, lat2) or a single point (long, lat).
#'   Defaults to TRUE. For single point, set to false.
#' @param \dots Additional args passed along to params portion of
#'    http request
#' @return Numeric vector with lat and long coordinates
#' @examples
#' \dontrun{
#' lookup_coords("san francisco, CA", "country:US")
#' }
#' @importFrom jsonlite fromJSON
#' @export
lookup_coords <- function(address, components = NULL, box = TRUE, ...) {
    if (missing(address)) stop("must supply address", call. = FALSE)
    stopifnot(is.atomic(address), is.atomic(components))
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
    ## extract, name, and then format
    coords <- rev(unlist(r$results$geometry[1, 1], use.names = FALSE))
    names(coords) <- c("long1", "lat1", "long2", "lat2")
    if (box) {
        coords
    } else {
        c(long = mean(coords[c(1, 3)]),
          lat = mean(coords[c(2, 4)]))
    }
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

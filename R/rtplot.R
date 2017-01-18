#' lookup_coords
#'
#' Returns lat long coordinates using google geocode api
#'
#' @param address Desired location, e.g., \code{"lawrence, KS"}
#' @param components Unit of analysis for address e.g.,
#'    \code{"country:US"}. Potential components include postal_code,
#'    country, administrative_area, locality, route.
#' @param \dots Additional args passed along to params portion of
#'    http request
#' @return Numeric vector with lat and long coordinates
#' @examples
#' \dontrun{
#' lookupcoords("san francisco, CA", "country:US")
#' }
#' @importFrom jsonlite fromJSON
#' @export
lookup_coords <- function(address, components = NULL, ...) {
    if (missing(address)) stop("must supply address", call. = FALSE)
    stopifnot(is.atomic(address), is.atomic(components))

    address <- gsub(" ", "+", gsub(",", "", address))

    params <- list(address = address,
                   components = components, ...)
    params <- params[!vapply(params, is.null, logical(1))]
    params <- paste0(
        mapply(function(x, y) paste0(x, "=", y),
               names(params), params),
        collapse = "&")

    geourl <- paste0("https://maps.googleapis.com/maps/api/geocode/json?",
                     params)
    r <- readLines(geourl)
    r <- jsonlite::fromJSON(r)
    rev(unlist(r$results$geometry[1, 1], use.names = FALSE))
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

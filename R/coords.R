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
    lns <- vapply(coordinates, length, integer(1))
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

  if (any(is.data.frame(coordinates), is.matrix(coordinates))) {
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
  if (requireNamespace("tibble", quietly = TRUE)) {
    latlng <- tibble::as_tibble(latlng)
  }
  latlng
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
setMethod("$", "coords", function(x, name) slot(x, name))

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


setMethod("show", "gcoords", function(object) print(object))

#' print gcoords
#'
#' @param x Object of class coords.
#' @param n Number of obs to return
#' @param digits Number of digits to print.
print.gcoords <- function(x, n = 10, digits = 2) {
  message("An object of class \"gcoords\"")
  message("\nBounding box:")
  b <- apply(
    x@box[1:n, ], 2,
    function(.) as.double(round(., digits = digits))
  )
  print(b)
  message("\nPoint:")
  p <- apply(
    x@point[1:n, ], 2,
    function(.) as.double(round(., digits = digits))
  )
  print(p)
}

setMethod("$", "gcoords", function(x, name) slot(x, name))



gcoords <- setClass(
  "gcoords",
  slots = c(
    point = "matrix",
    box = "matrix")
)



as_gcoords <- function(point, box) {
  ## point
  if (!all(is.matrix(point), is.numeric(point))) {
    if (is.atomic(point)) {
      point <- strsplit(point, "\\s", perl = TRUE)
    }
    if (is.list(point)) {
      stopifnot(all(lengths(point) < 3L))
      if (!all(lengths(point) == 2L)) {
        point[is.na(point)] <- rep(list(rep(NA, 2)), sum(is.na(point)))
      }
      point <- matrix(
        unlist(point), length(point), 2L, byrow = TRUE)
    }
    if (is.data.frame(point)) {
      point <- as.matrix(point)
    }
    point <- apply(point, 2, as.double)
  }

  ## box
  if (!all(is.matrix(box), is.numeric(box))) {
    if (is.atomic(box)) {
      box <- strsplit(box, "\\s", perl = TRUE)
    }
    if (is.list(box)) {
      stopifnot(lengths(box) < 9L)
      if (!all(lengths(box) == 8L)) {
        box[is.na(box)] <- rep(list(rep(NA, 8)), sum(is.na(box)))
      }
      box <- matrix(
        unlist(box), length(box), 8L, byrow = TRUE
      )
    }
    if (is.data.frame(box)) {
      box <- as.matrix(box)
    }
    box <- apply(box, 2, as.double)
  }
  gcoords(point = point, box = box)
}


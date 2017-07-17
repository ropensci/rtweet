parser.placebb <- function(x) {
  if (length(x$place) == 0) {
    x <- rep(NA, nrow(x))
  } else if (is.atomic(x$place)) {
    x <- rep(NA, nrow(x))
  } else if (length(x$place$bounding_box) == 0) {
    x <- rep(NA, nrow(x))
  } else {
    return(parser(x$place, "bounding_box"))
  }
  parser(as_coordinates(x))
}

help_page <- function(q) {
  paste0("http://127.0.0.1:10760/doc/html/Search?pattern=", q,
         "&fields.alias=1&fields.title=1&fields.concept=1&ignore.case=1",
         "&types.help=1&types.vignette=1&types.demo=1")
}

help_page("oldClass")

as_placebb <- function(x) {
  class(x) <- c("placebb", "data.frame")
  x
}


as_coordinates <- function(x) {
  if (!inherits(x, "data.frame")) {
    if (isTRUE(all(c("type", "coordinates") %in%
                   names(x), na.rm = TRUE))) {
      x <- data.frame(
        type = x$type,
        coordinates = I(list(x$coordinates))
      )
    } else {
      if (!is.atomic(x)) {
        x[lengths(x) == 0] <- NA
        x[lengths(x) > 1L] <- NA
      }
      x <- data.frame(x)
    }
  }
  class(x) <- c("coordinates", "data.frame")
  x
}


parser.coordinates <- function(x, ...) {
  if (any(x$type == "Point", na.rm = TRUE)) {
    x <- x$coordinates
    x[lengths(x) == 0] <- list(rep(NA_real_, 2))
    x <- do.call("rbind", x)
  } else if (any(x$type == "Polygon", na.rm = TRUE)) {
    x <- x$coordinates
    x[lengths(x) == 8L] <- lapply(
      x[lengths(x) == 8L], function(a) {
        a <- a[1, , ]
        lng <- mean(a[, 1], na.rm = TRUE)
        lat <- mean(a[, 2], na.rm = TRUE)
        c(lng, lat)
      }
    )
    x[lengths(x) == 0] <- list(rep(NA_real_, 2))
    x <- do.call("rbind", x)
  } else {
    x <- matrix(NA_real_, nrow(x), 2)
  }
  structure(
    data.frame(x), names = c("lng", "lat")
  )
}

#' parser.data.frame
#'
#' Converts twitter data frame to named element, NA vector, or nested
#'   data frame.
#'
#' @param x A data frame converted from Twitter json object.
#' @param i Optional, variable name to extract.
#' @return This is a top-level function that either returns a vector, the
#'   appropriate number of NAs, or a data frame.
#' @noRd
parser.data.frame <- function(x, i = NULL) {
  if (is.null(x)) {
    NA
  } else if (isTRUE(NROW(x) == 0L)) {
    NA
  } else if (isTRUE(all(c("type", "coordinates") %in% names(x)))) {
    parser(as_coordinates(x))
  } else if (is.null(i)) {
    x
  } else if (any(i %in% c("coordinates", "geo", "bounding_box"))) {
    parser(as_coordinates(x[[i]]))
  } else if (length(x[[i]]) > 0L) {
    x[[i]]
  } else {
    NA
  }
}

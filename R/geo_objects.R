# Contains functions to parse the objects described here: 
# https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/object-model/geo

bounding_box <- function(x) {
  empty <- data.frame(long = NA, lat = NA, type = NA)
  if (is.null(x) || (identical(x, NA))) {
    return(empty)
  }
  if (is.data.frame(x)) {
    coord <- x$coordinates[[1]][1, , ]
    if (is.null(coord)) {
      return(empty)
    }
    return(data.frame(long = coord[, 1], lat = coord[, 2], type = x$type))
  }
  m <- x$coordinates[1, , ]
  colnames(m) <- c("long", "lat")
  df <- as.data.frame(m)
  df$type <- x$type
  df
}

# <https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/object-model/geo#coordinates>
coordinates <- function(x) {
  if (is.null(x) || length(x) == 1 && is.na(x)) {
    return(data.frame(long = NA, lat = NA, type = NA))
  }
  if (has_name_children(x, "coordinates", "coordinates")) {
    return(data.frame(long = x$coordinates[[1]], lat = x$coordinates[[2]], type = x$type))
  }
  if (is.logical(x)) {
    nas <- rep(NA, length(x))
    return(data.frame(long = nas, lat = nas, type = nas))
  }
  if (is.data.frame(x)) {
    pos_coord <- function(y, pos){
      if (is.null(y)) {
        NA_real_
      } else {
        y[pos]
      }
    }
    long <- vapply(x$coordinates, pos_coord, numeric(1L), pos = 1)
    lat <- vapply(x$coordinates, pos_coord, numeric(1L), pos = 2)
    
    return(data.frame(long = long, lat = lat, type = x$type))
  }

  data.frame(long = x$coordinates[[1]], lat = x$coordinates[[2]], type = x$type)
}

# <https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/object-model/geo#place>
place <- function(x) {
  if (is.null(x) || length(x) == 1 && is.na(x)) {
    df <- data.frame(geo = I(list(coordinates(NA))), 
                     coordinates = I(list(coordinates(NA))),
                             place = I(list(NA)))
    return(df)
  }

  if (is.data.frame(x)) {
    l <- simplify2array(x[!colnames(x) %in% c("geo", "coordinates", "bounding_box")])
  } else if (is.list(x)) {
    l <- simplify2array(x[!names(x) %in% c("geo", "coordinates", "bounding_box")])
    if (nrow(l) != 1) {
      l <- t(l)
    }
  }
  place <- as.data.frame(l)
  place$bounding_box <- list(bounding_box(x$bounding_box))
  
  data.frame(geo = I(coordinates(x$geo)),
            coordinates = I(coordinates(x$coordinates)),
            place = I(place))
}

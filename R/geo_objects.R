# Contains functions to parse the objects described here: 
# https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/object-model/geo

bounding_box <- function(x) {
  if(is.null(x)) {
    as_tbl(data.frame(long = NA, lat = NA, type = NA))
  }
  
  m <- x$coordinates[1, , ]
  colnames(m) <- c("long", "lat")
  df <- as.data.frame(m)
  df$type <- x$type
  as_tbl(df)
}

# <https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/object-model/geo#coordinates>
coordinates <- function(x) {
  if (is.null(x) || length(x) == 1 && is.na(x)) {
    return(as_tbl(data.frame(long = NA, lat = NA, type = NA)))
  }
  if (has_name_children(x, "coordinates", "coordinates")) {
    return(as_tbl(data.frame(long = x$coordinates[[1]], lat = x$coordinates[[2]], type = x$type)))
  }
  as_tbl(data.frame(long = x[[1]], lat = x[[2]], type = x$type))
}

# <https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/object-model/geo#place>
place <- function(x) {
  if (is.null(x)) {
    df <- data.frame(geo = I(list(NA)), coordinates = I(list(NA)),
                             place = I(list(NA)))
    return(as_tbl(df))
  }
  l <- simplify2array(x$place[!names(x$place) %in% "bounding_box"])
  l[lengths(l) == 0] <- NA
  place <- as.data.frame(l)
  place$bounding_box <- list(bounding_box(x$place$bounding_box))
  
  tibble::tibble(geo = coordinates(x$geo),
            coordinates = coordinates(x$coordinates),
            place = place)
}

# Contains functions to parse the objects described here: 
# https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/object-model/geo

bounding_box <- function(x) {
  if(is.null(x)) {
    data.frame(long = NA, lat = NA, type = NA)
  }
  
  m <- x$coordinates[1, , ]
  colnames(m) <- c("long", "lat")
  df <- as.data.frame(m)
  df$type <- x$type
  df
}

# <https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/object-model/geo#coordinates>
geo <- function(x) {
  if (is.null(x)) {
    return(data.frame(long = NA, lat = NA, type = NA))
  }
  
  data.frame(long = x$coordinates[[1]], lat = x$coordinates[[2]], type = x$type)
}

# <https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/object-model/geo#place>
place <- function(x) {
  if (is.null(x)) {
    df <- data.frame(geo = I(list(NA)), coordinates = I(list(NA)),
                             place = I(list(NA)))
    return(df)
  }
  l <- simplify2array(x$place[!names(x$place) %in% "bounding_box"])
  l[lengths(l) == 0] <- NA
  place <- as.data.frame(l)
  place$bounding_box <- list(bounding_box(x$place$bounding_box))
  
  tibble::tibble(geo = geo(x$geo),
            coordinates = geo(x$coordinates),
            place = place)
}

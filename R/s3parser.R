

#' parser method
#'
#' Wrangling methods for converting nested lists (supplied by Twitter)
#'   into data frames
#'
#' @param x Data object.
#' @param \dots Args passed along to various methods. Basically, it's used to
#'   specify the desired variable to extract.
#' @return Parsed data object.
#' @examples
#' \dontrun{
#' rt <- search_tweets("lang:en", n = 500, parse = FALSE)
#' df <- parser(rt)
#' df
#' users_data(df)
#' }
#' @export
parser <- function(x, ...) UseMethod("parser")

parser.default <- function(x, ...) x

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

#' parser.coordinates
#'
#' Converts coordinates data frame to 2 X N data frame with lng and lat
#'   coordinates respectively.
#'
#' @param x Object with "type" and "coordinates" named elements. This appears in
#'   twitter objects labelled, "coordinates", "geo", and "place.bounding_box".
#' @param \dots Dots.
#' @return Data frame with lng and lat numeric columns.
#' @noRd
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

#' parser.list
#'
#' Converts twitter data frame to named element, NA vector, or nested
#'   data frame.
#'
#' @param x A data frame converted from Twitter json object.
#' @param i Optional, variable name to extract.
#' @return This is a top-level function that either returns a vector, the
#'   appropriate number of NAs, or a data frame.
#' @noRd
parser.list <- function(x, i = NULL) parser.data.frame(x, i)

#' parser.NULL
#'
#' Side-steps default behavior when dealing with NULL elements to return
#'   NAs instead.
#'
#' @param x This does't really matter
#' @param i Neither does this really.
#' @return This is a top-level function that either returns a vector, the
#'   appropriate number of NAs, or a data frame.
#' @noRd
parser.NULL <- function(x, i) NA


as_false <- function(x, i) {
  if (length(x[[i]]) == 0) {
    x <- rep(NA, nrow(x))
  } else {
    x <- x[[i]]
  }
  if (!is.data.frame(x)) {
    x <- data.frame(x)
  }
  class(x) <- c("false", "data.frame")
  x
}

as_not_false <- function(x, i) {
  if (length(x[[i]]) == 0) {
    x <- rep(NA, nrow(x))
  } else {
    x <- x[[i]]
  }
  if (!is.data.frame(x)) {
    x <- data.frame(x)
  }
  class(x) <- c("not_false", "data.frame")
  x
}


parser.false <- function(x, i) {
  lgl <- rep(FALSE, nrow(x))
  if (length(x[[i]]) == 0L) {
    lgl
  } else {
    lgl[!is.na(x[[i]])] <- TRUE
  }
  lgl
}

parser.not_false <- function(x, i) {
  na <- rep(NA, nrow(x))
  if (length(x[[i]]) == 0L) {
    na
  } else {
    na[!is.na(x[[i]])] <- x[[i]][!is.na(x[[i]])]
  }
  na
}


#' parser.statuses
#'
#' Converts statuses Twitter object to tidy[er] data frame.
#'
#' @param x A data frame of class "statuses."
#' @return A tibble data frame of tweets data.
#' @importFrom tibble as_tibble
#' @noRd
parser.statuses <- function(x) {
  class(x) <- "data.frame"
  geo <- parser(x, "geo")
  coordinates <- parser(x, "coordinates")
  bounding_box <- parser.placebb(x)
  if (inherits(x$user, "no_user")) {
    users <- x$user
  } else {
    users <- parser.user(x$user)
  }
  data <- as_tibble(list(
    status_id = parser(x, "id_str"),
    created_at = format_date(as.character(parser(x, "created_at"))),
    text = parser(x, "text"),
    user_id = parser(x$user, "id_str"),
    screen_name = parser(x$user, "screen_name"),
    retweet_count = parser(x, "retweet_count"),
    favorite_count = parser(x, "favorite_count"),
    source = parser(x$user, "source"),
    lang = parser(x, "lang"),
    reply_to_status_id = parser(x, "in_reply_to_status_id"),
    reply_to_user_id = parser(x, "in_reply_to_user_id"),
    reply_to_screen_name = parser(x, "in_reply_to_screen_name"),
    ##full_text = parser(x, "full_text"),
    symbols = lapply(x$entities$symbols, "parser", "text"),
    mentions_screen_name = lapply(x$entities$user_mentions, "parser", "screen_name"),
    mentions_user_id = lapply(x$entities$user_mentions, "parser", "id_str"),
    media_url = lapply(x$entities$media, "parser", "url"),
    media_expanded_url = lapply(x$entities$media, "parser", "expanded_url"),
    media_type = lapply(x$entities$media, "parser", "type"),
    hashtags = lapply(x$entities$hashtags, "parser", "text"),
    is_retweet = parser(as_false(x, "retweeted_status"), "id_str"),
    retweet_status_id = parser(as_not_false(x, "retweeted_status"), "id_str"),
    is_quote = parser(as_false(x, "quoted_status"), "id_str"),
    quote_status_id = parser(as_not_false(x, "quoted_status"), "id_str"),
    geo_lng = geo$lng,
    geo_lat = geo$lat,
    coordinates_lng = coordinates$lng,
    coordinates_lat = coordinates$lat,
    place_name = parser(x$place, "full_name"),
    place_type = parser(x$place, "place_type"),
    country = parser(x$place, "country"),
    place_url = parser(x$place, "url"),
    bounding_box_lng = bounding_box$lng,
    bounding_box_lat = bounding_box$lat),
    validate = FALSE
  )
  attr(data, "users") <- users
  data
}

#' parsestatuses
#'
#' @param x List of returned Twitter data containing object statuses.
#' @return Tibble data frame with users attribute.
#' @noRd
parsestatuses <- function(x) {
  if (all(vapply(x, has_var, i = "statuses", logical(1)))) {
    x <- lapply(x, "[[", "statuses")
  }
  x <- lapply(x, parser.statuses)
  users <- do.call("rbind", lapply(x, users_data))
  x <- do.call("rbind", x)
  attr(x, "users") <- users
  x
}

parser.tweets <- function(x) {
  users <- do.call("rbind", lapply(x, users_data))
  x <- do.call("rbind", x)
  attr(x, "users") <- users
  x
}

parser.tweet <- function(x) {
  x <- do.call("rbind", x)
  x
}

parse_timelines <- function(x) {
  x <- lapply(x, parser.statuses)
  users <- do.call("rbind", lapply(x, users_data))
  x <- do.call("rbind", x)
  attr(x, "users") <- users
  x
}

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

as_placebb <- function(x) {
  class(x) <- c("placebb", "data.frame")
  x
}

as_tweets <- function(x) {
  if (all(vapply(x, has_var, i = "statuses", logical(1)))) {
    x <- lapply(x, "[[", "statuses")
  }
  x <- lapply(x, `class<-`, c("statuses", "data.frame"))
  x <- lapply(x, function(i) UseMethod(generic = "parser", i))
  class(x) <- c("tweets", "list")
  x
}

as_tweet <- function(x) {
  x <- lapply(x, as_status)
  x <- lapply(x, function(i) UseMethod(generic = "parser", i))
  class(x) <- c("tweet", "list")
  x
}

as_users <- function(x) {
  x <- lapply(x, as_user)
  x <- lapply(x, function(i) UseMethod("parser", i))
  class(x) <- c("users", "list")
  x
}

as_status <- function(x) {
  id_str <- x$id_str
  screen_name <- x$screen_name
  x <- x$status
  x$user <- data.frame(
    id_str, screen_name, stringsAsFactors = FALSE
  )
  class(x$user) <- c("no_user", "data.frame")
  class(x) <- c("statuses", "data.frame")
  x
}

parser.users <- function(x) {
  users <- do.call("rbind", lapply(x, users_data))
  x <- do.call("rbind", x)
  attr(x, "users") <- users
  x
}


as_userenturl <- function(x) {
  class(x) <- c("userenturl", "data.frame")
  x
}

parser.userenturl <- function(x, i) {
  if (!has_var(x, "entities") ||
      !has_var(x$entities, "url") ||
      !has_var(x$entities$url, "urls")) {
    x <- as.list(rep(NA, nrow(x)))
  } else {
    x <- x$entities$url$urls
  }
  x <- lapply(x, "parser", i)
  if (any(lengths(x) > 1L, na.rm = TRUE)) {
    x[lengths(x) > 1L] <- vapply(
      x[lengths(x) > 1L], paste, collapse = " ",
      character(1), USE.NAMES = FALSE)
  }
  unlist(x, use.names = FALSE)
}

as_user <- function(x) {
  if ("user" %in% names(x)) {
    x <- x$user
  }
  class(x) <- c("user", "data.frame")
  x
}

#' parser user
#'
#' Converts Twitter user object into tidy data frame.
#'
#' @param x Twitter data user object or list containing users data frame.
#' @return Tibble data frame.
#' @importFrom tibble as_tibble
parser.user <- function(x) {
  class(x) <- "data.frame"
  ueu <- as_userenturl(x)
  as_tibble(list(
    user_id = parser(x, "id_str"),
    screen_name = parser(x, "screen_name"),
    name = parser(x, "name"),
    created_at = format_date(parser(x, "created_at")),
    location = parser(x, "location"),
    description = parser(x, "description"),
    followers_count = parser(x, "followers_count"),
    friends_count = parser(x, "friends_count"),
    favourites_count = parser(x, "favourites_count"),
    listed_count = parser(x, "listed_count"),
    statuses_count = parser(x, "statuses_count"),
    lang = parser(x, "lang"),
    verified = parser(x, "verified"),
    profile_url = parser(ueu, "url"),
    profile_expanded_url = parser(ueu, "expanded_url"),
    profile_background_color = parser(x, "profile_background_color"),
    profile_background_tile = parser(x, "profile_background_tile"),
    profile_image_url = parser(x, "profile_image_url"),
    profile_image_url_https = parser(x, "profile_image_url_https"),
    profile_link_color = parser(x, "profile_link_color"),
    profile_banner_url = parser(x, "profile_banner_url"),
    profile_text_color = parser(x, "profile_text_color"),
    profile_use_background_image = parser(x, "profile_use_background_image"),
    default_profile = parser(x, "default_profile"),
    default_profile_image = parser(x, "default_profile_image")),
    validate = FALSE
  )
}

has_var <- function(x, i) {
  if (is.atomic(x)) return(FALSE)
  if (isTRUE(i %in% names(x))) {
    TRUE
  } else {
    FALSE
  }
}


parser.s2p <- function(x) {
  x <- lapply(x, "[[", "statuses")
  x <- lapply(
    x, function(i) {
      class(i) <- c("statuses", "data.frame")
    i
  })
  x <- lapply(x, parser)
  df <- do.call("rbind", x)
  attr(df, "users") <- do.call("rbind", lapply(x, attr, "users"))
  df
}

parser.s2p2 <- function(x) {
  for (i in seq_along(x)) {
    x[[i]] <- x[[i]]$statuses
    class(x[[i]]) <- c("statuses", "data.frame")
    x[[i]] <- parser(x[[i]])
  }
  df <- do.call("rbind", x)
  attr(df, "users") <- do.call("rbind", lapply(x, attr, "users"))
  df
}

as_s2p <- function(x) {
  x <- lapply(x, "[[", "statuses")
  lapply(x, function(i) return(class(i) <- "statuses"))
}


flatten_df <- function(x) {
  recs <- !unlist(lapply(x, is.atomic))
  recs <- names(x)[which(recs)]
  for (i in recs) {
    x[[i]][is.na(x[[i]])] <- NA_character_
    x[[i]][!is.na(x[[i]])] <- vapply(
      x[[i]][!is.na(x[[i]])], paste, collapse = " ",
      character(1)
    )
    x[[i]] <- unlist(x[[i]])
  }
  x
}

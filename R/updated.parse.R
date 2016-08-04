#' .rna
#'
#' @export
.rna <- function(x, n) {
  if (is.null(x) | identical(x, "")) {
    x <- rep(NA, n)
  }
  x[x == ""] <- NA
  x
}

#' .ret_ply
#'
#' @export
.ret_ply <- function(x, n = NULL) {
  toplevel <- c(
    "created_at", "id_str", "retweet_count", "favorite_count",
    "text", "in_reply_to_status_id_str", "in_reply_to_user_id_str",
    "is_quote_status", "quoted_status_id_str", "lang")

  if (is.null(n)) n <- length(x[["id_str"]])

  ret_df <- lapply(toplevel, function(i) .rna(x[[i]], n))

  names(ret_df) <- toplevel
  if ("created_at" %in% names(ret_df)) {
    ret_df[["created_at"]] <- as.Date(as.POSIXct(
      ret_df[["created_at"]],
      format = "%a %b %d %H:%M:%S %z %Y"))
  }
  dplyr::tbl_df(ret_df)
}

#' .make_entities
#'
#' @export
.make_entities <- function(dat, n = NULL) {

  if (is.null(n)) n <- length(dat[["id_str"]])

  if (!"entitites" %in% names(dat)) {
    ent_df <- dplyr::data_frame(
      user_mentions = as.list(rep(NA_character_, n)),
      hashtags = as.list(rep(NA_character_, n)),
      urls = as.list(rep(NA_character_, n)))

  } else {
    entities <- dat[["entities"]]

    user_mentions <- .rna(entities[["user_mentions"]], n)
    hashtags <- .rna(entities[["hashtags"]], n)
    urls <- .rna(entities[["urls"]], n)

    ent_df <- dplyr::data_frame(
      user_mentions = lapply(user_mentions, function(i) .rna(i[["id_str"]], 1)),
      hashtags = lapply(hashtags, function(i) .rna(i[["text"]], 1)),
      urls = lapply(urls, function(i) .rna(i[["expanded_url"]], 1)))
  }

  ent_df
}

#' .make_retweeted
#'
#' @export
.make_retweeted <- function(dat, n = NULL) {
  if (is.null(n)) n <- length(dat[["id_str"]])

  if (!"retweeted_status" %in% names(dat)) {
    retweeted_df <- dplyr::data_frame(
      is_retweet = rep(NA, n),
      retweet_status_id = rep(NA_character_, n))
  } else {
    retweeted_status <- dat[["retweeted_status"]]

    retweeted_df <- dplyr::data_frame(
      is_retweet = !is.na(.rna(retweeted_status[["id_str"]], n)),
      retweet_status_id = .rna(retweeted_status[["id_str"]], n))
  }
  retweeted_df
}

#' .make_coords
#'
#' @export
.make_coords <- function(x) {
  x <- data.frame(matrix(as.numeric(x), 1, 8))
  names(x) <- c(
    "long1", "long2", "long3", "long4",
    "lat1", "lat2", "lat3", "lat4")
  x
}

#' .place_nadf
#'
#' @export
.place_nadf <- function(n, all = TRUE) {
  if (all) {
    place_df <- dplyr::data_frame(
      place_name = rep(NA_character_, n),
      country = rep(NA_character_, n),
      long1 = rep(NA_character_, n),
      long2 = rep(NA_character_, n),
      long3 = rep(NA_character_, n),
      long4 = rep(NA_character_, n),
      lat1 = rep(NA_character_, n),
      lat2 = rep(NA_character_, n),
      lat3 = rep(NA_character_, n),
      lat4 = rep(NA_character_, n))
  } else {
    place_df <- dplyr::data_frame(
      long1 = rep(NA_character_, n),
      long2 = rep(NA_character_, n),
      long3 = rep(NA_character_, n),
      long4 = rep(NA_character_, n),
      lat1 = rep(NA_character_, n),
      lat2 = rep(NA_character_, n),
      lat3 = rep(NA_character_, n),
      lat4 = rep(NA_character_, n))
  }

  place_df
}

#' .make_place
#'
#' @export
.make_place <- function(dat, n = NULL) {
  if (is.null(n)) n <- length(dat[["id_str"]])

  if (!"place" %in% names(dat)) {
    place_df <- .place_nadf(n)
  } else {
    place <- dat[["place"]]

    if (!is.data.frame(place)) {
      place_df <- dplyr::data_frame(
        place_name = rep(NA_character_, n),
        place_country = rep(NA_character_, n))
    } else {
      place_df <- dplyr::data_frame(
        place_name = .rna(place[["full_name"]], n),
        place_country = .rna(place[["country"]], n))
    }

    if (!"bounding_box" %in% names(place)) {
      coordinates_df <- .place_nadf(n, all = FALSE)
    } else {
      bounding_box <- place[["bounding_box"]]

      if (!is.data.frame(bounding_box)) {
        coordinates_df <- .place_nadf(n, all = FALSE)
      } else
        if (!"coordinates" %in% names(bounding_box)) {
          coordinates_df <- .place_nadf(n, all = FALSE)
          } else {
          coordinates <- bounding_box[["coordinates"]]

          coordinates_df <- dplyr::bind_rows(lapply(coordinates, .make_coords))
          }
      }
    place_df <- dplyr::bind_cols(place_df, coordinates_df)
    }
  place_df
}


#' .return_statuses
#' @export
.return_statuses <- function(dat) {

  if ("statuses" %in% names(dat)) {
    dat <- dat[["statuses"]]
  }

  status_df <- dplyr::bind_cols(
    .ret_ply(dat),
    .make_entities(dat),
    .make_retweeted(dat),
    .make_place(dat),
    .make_user(dat))

  status_df
}

#' .user_ent
#' @export
.user_ent <- function(x) {
  if ("expanded_url" %in% names(x)) {
    return(as.character(x[["expanded_url"]]))
  } else {
    return(NA_character_)
  }
}

#' .make_user_url
#' @export
.make_user_url <- function(dat, n = NULL) {

  if (is.null(n)) n <- length(dat[["id_str"]])

  if ("user" %in% names(dat)) {
    user <- dat[["user"]]
    if ("entities" %in% names(user)) {
      entities <- user[["entities"]]
      if ("url" %in% names(entities)) {
        ent_url <- entities[["url"]]
        if ("urls" %in% names(ent_url)) {
          urls <- ent_url[["urls"]]
          expanded_url <- lapply(urls, .user_ent)
        }
      }
    }
  }
  else {
    expanded_url <- as.list(rep(NA_character_, n))
  }
  expanded_url
}

#' .make_user
#' @export
.make_user <- function(dat, n = NULL) {

  toplevel <- c(
    "id_str", "name", "screen_name", "location", "description",
    "url", "protected", "followers_count", "friends_count", "listed_count",
    "created_at", "favourites_count", "utc_offset", "time_zone",
    "geo_enabled", "verified", "statuses_count", "lang")

  if (is.null(n)) n <- length(dat[["id_str"]])

  if (!"user" %in% names(dat)) {
    user_df <- dplyr::data_frame(
      user_id = rep(NA_character_, n),
      name = rep(NA_character_, n),
      screen_name = rep(NA_character_, n),
      location = rep(NA_character_, n),
      description = rep(NA_character_, n),
      user_url = rep(NA_character_, n),
      protected = rep(NA, n),
      followers_count = rep(NA_integer_, n),
      friends_count = rep(NA_integer_, n),
      listed_count = rep(NA_integer_, n),
      account_created_at = rep(NA_character_, n),
      user_favourites_count = rep(NA_integer_, n),
      utc_offset = rep(NA_character_, n),
      time_zone = rep(NA_character_, n),
      geo_enabled = rep(NA, n),
      verified = rep(NA, n),
      statuses_count = rep(NA_integer_, n),
      user_lang = rep(NA, n))
  } else {
    user <- dat[["user"]]
    user_df <- lapply(toplevel, function(i) .rna(user[[i]], n))
    names(user_df) <- c(
      "user_id", "name", "screen_name", "location",
      "description", "user_url", "protected", "followers_count",
      "friends_count", "listed_count", "account_created_at",
      "user_favourites_count", "utc_offset", "time_zone",
      "geo_enabled", "verified", "statuses_count", "user_lang")
    user_df <- dplyr::tbl_df(user_df)
  }
  user_df
}

#' return_with_NA
#'
#' @export
return_with_NA <- function(x, n) {
  if (is.character(x)) {
    myNA <- NA_character_
  } else if (is.logical(x)) {
    myNA <- NA
  } else if (is.integer(x)) {
    myNA <- NA_integer_
  } else {
    myNA <- NA_character_
  }
  if (any(is.null(x), identical(x, ""))) {
    x <- rep(NA, n)
  }
  for (i in seq_along(x)) {
    if (any(is.null(x[[i]]), identical(x[[i]], ""))) {
      x[[i]] <- NA
    }
  }
  x
}

#' return_with_NA
#'
#' @export
check_response_obj <- function(x) {
  if (all(c("statuses", "search_metadata") %in% names(x))) {
    x <- x[["statuses"]]
  }

  if (!"id_str" %in% names(x)) {
    if ("id" %in% names(x)) {
      x$id_str <- x$id
    } else {
      stop("object does not contain ID variable.", call. = FALSE)
    }
  }
  x
}

#' statuses_toplevel_df
#'
#' @export
statuses_toplevel_df <- function(x, n = NULL) {

  toplevel <- c(
    "created_at", "id_str", "retweet_count", "favorite_count",
    "text", "in_reply_to_status_id_str", "in_reply_to_user_id_str",
    "is_quote_status", "quoted_status_id_str", "lang")

  x <- check_response_obj(x)

  if (is.null(n)) n <- length(x[["id_str"]])

  toplevel_df <- lapply(x[toplevel], return_with_NA)
  toplevel_df$user_id <- statuses_user_id(x)

  if ("created_at" %in% names(toplevel_df)) {
    toplevel_df[["created_at"]] <- format_date(
      toplevel_df[["created_at"]], date = FALSE)
  }

  dplyr::tbl_df(toplevel_df)
}


#' statuses_entities_df
#'
#' @export
statuses_entities_df <- function(dat, n = NULL) {

  dat <- check_response_obj(dat)

  if (is.null(n)) n <- length(dat[["id_str"]])

  ent_df <- dplyr::data_frame(
    user_mentions = as.list(rep(NA_character_, n)),
    hashtags = as.list(rep(NA_character_, n)),
    urls = as.list(rep(NA_character_, n)))

  if ("entities" %in% names(dat)) {
    entities <- dat[["entities"]]

    if ("user_mentions" %in% names(entities)) {
      ent_df$user_mentions <- lapply(entities[["user_mentions"]],
        function(x) return_with_NA(x[["id_str"]], 1))
    }

    if ("hashtags" %in% names(entities)) {
      ent_df$hashtags <- lapply(entities[["hashtags"]],
        function(x) return_with_NA(x[["text"]], 1))
    }

    if ("urls" %in% names(entities)) {
      ent_df$urls <- lapply(entities[["urls"]],
        function(x) return_with_NA(x[["expanded_url"]], 1))
    }
  }

  ent_df
}

#' statuses_retweet_df
#'
#' @export
statuses_retweet_df <- function(dat, n = NULL) {

  dat <- check_response_obj(dat)

  if (is.null(n)) n <- length(dat[["id_str"]])

  retweet_df <- dplyr::data_frame(
    is_retweet = rep(NA, n),
    retweet_status_id = rep(NA_character_, n))

  if (is.null(n)) n <- length(dat[["id_str"]])

  if ("retweeted_status" %in% names(dat)) {
    retweeted_status <- dat[["retweeted_status"]]

    retweet_df$is_retweet = !is.na(return_with_NA(
      retweeted_status[["id_str"]], n))

    retweet_df$retweet_status_id = return_with_NA(
      retweeted_status[["id_str"]], n)
  }

  retweet_df
}

#' .make_coords
#'
#' @export
.make_coords <- function(x) {

  if (is.array(x)) {
    coords_df <- data.frame(matrix(as.numeric(x), 1, 8))
    names(coords_df) <- c(
      "long1", "long2", "long3", "long4",
      "lat1", "lat2", "lat3", "lat4")
  } else {
    coords_df <- data.frame(matrix(rep(NA_real_, 8), 1, 8))

    names(coords_df) <- c(
      "long1", "long2", "long3", "long4",
      "lat1", "lat2", "lat3", "lat4")
  }

  coords_df
}

#' statuses_place_df
#'
#' @export
statuses_place_df <- function(dat, n = NULL) {

  dat <- check_response_obj(dat)

  if (is.null(n)) n <- length(dat[["id_str"]])

  place_df <- dplyr::data_frame(
    place_name = rep(NA_character_, n),
    country = rep(NA_character_, n),
    long1 = rep(NA_real_, n),
    long2 = rep(NA_real_, n),
    long3 = rep(NA_real_, n),
    long4 = rep(NA_real_, n),
    lat1 = rep(NA_real_, n),
    lat2 = rep(NA_real_, n),
    lat3 = rep(NA_real_, n),
    lat4 = rep(NA_real_, n))

  if ("place" %in% names(dat)) {
    place <- dat[["place"]]

    if ("full_name" %in% names(place)) {
      place_df$place_name = return_with_NA(place[["full_name"]], n)
    }
    if ("country" %in% names(place)) {
      place_df$country = return_with_NA(place[["country"]], n)
    }

    if ("bounding_box" %in% names(place)) {
      bounding_box <- place[["bounding_box"]]

      if ("coordinates" %in% names(bounding_box)) {
        coordinates <- bounding_box[["coordinates"]]
        coordinates <- lapply(coordinates, .make_coords)

        for (i in seq_along(coordinates)) {
          place_df[i, 3:10] <- coordinates[[i]]
        }
      }
    }
  }
  place_df
}

statuses_user_id <- function(dat, n = NULL) {

  dat <- check_response_obj(dat)

  if (is.null(n)) n <- length(dat[["id_str"]])

  user_id <- rep(NA_character_, n)

  if ("user" %in% names(dat)) {
    user <- dat[["user"]]

    if ("id_str" %in% names(user)) {
      user_id <- user[["id_str"]]
    }
  }

  user_id
}




#' statuses_df
#' @export
statuses_df <- function(dat) {

  if ("statuses" %in% names(dat)) {
    dat <- dat[["statuses"]]
  }

  status_df <- dplyr::bind_cols(
    statuses_toplevel_df(dat),
    statuses_entities_df(dat),
    statuses_retweet_df(dat),
    statuses_place_df(dat))

  status_df
}

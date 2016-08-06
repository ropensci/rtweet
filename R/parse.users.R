check_user_obj <- function(x) {

  if ("user" %in% names(x)) {
    x <- x[["user"]]
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

usr_ent_urls <- function(x, list = FALSE) {

  if (is.data.frame(x)) {
    if ("expanded_url" %in% names(x)) {
      if (list) {
        x <- as.character(x[["expanded_url"]])
      } else {
        x <- x[["expanded_url"]]
      }
    } else {
      x <- NA_character_
    }
  } else {
    x <- NA_character_
  }

  x
}


user_toplevel_df <- function(x, n = NULL) {

  toplevel <- c(
    "id_str", "name", "screen_name", "location", "description",
    "url", "protected", "followers_count", "friends_count", "listed_count",
    "created_at", "favourites_count", "utc_offset", "time_zone",
    "geo_enabled", "verified", "statuses_count", "lang")

  x <- check_response_obj(x)

  if (is.null(n)) n <- length(x[["id_str"]])

  toplevel_df <- lapply(x[toplevel], return_with_NA)

  toplevel_df$user_id <- check_user_id(x)

  if ("created_at" %in% names(toplevel_df)) {
    toplevel_df[["created_at"]] <- format_date(
      toplevel_df[["created_at"]], date = FALSE)
  }

  dplyr::tbl_df(toplevel_df)
}


#' user_entities_df
#' @export
user_entities_df <- function(dat, n = NULL) {

  dat <- check_user_obj(dat)

  if (is.null(n)) n <- length(dat[["id_str"]])

  user_ent_df <- dplyr::data_frame(
    url = rep(NA_character_, n),
    description_urls = as.list(rep(NA_character_, n)))

  if ("entities" %in% names(dat)) {
    entities <- dat[["entities"]]

    if ("url" %in% names(entities)) {
      ent_url <- entities[["url"]]

      if ("urls" %in% names(ent_url)) {
        user_ent_df$url <- unlist(lapply(
          ent_url[["urls"]], usr_ent_urls))
        }
      }

      if ("description" %in% names(entities)) {
        ent_url <- entities[["description"]]

        if ("urls" %in% names(ent_url)) {
          user_ent_df$description_urls <- lapply(
            ent_url[["urls"]], usr_ent_urls)
        }
      }

  }
  user_ent_df
}


#' user_df
#' @export
user_df <- function(dat) {

  if ("user" %in% names(dat)) {
    dat <- dat[["user"]]
  }

  user_df <- dplyr::bind_cols(
    user_toplevel_df(dat),
    user_entities_df(dat))

  user_df
}


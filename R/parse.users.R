
usr_ent_urls <- function(x, list = FALSE) {

  if (is.data.frame(x)) {
    if ("expanded_url" %in% names(x)) {
      if (list) {
        x <- as.list(x[["expanded_url"]])
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

user_toplevel_df <- function(x, n = NULL, names = NULL,
                             add.names = NULL) {

  if (is.null(names)) {
    toplevel <- c("id_str", "name", "screen_name",
      "location", "description", "url", "protected",
      "followers_count", "friends_count", "listed_count",
      "created_at", "favourites_count", "utc_offset",
      "time_zone", "geo_enabled", "verified",
      "statuses_count", "lang")
  }

  if (!is.null(add.names)) {
    toplevel <- c(toplevel, add.names)
  }

  x <- check_response_obj(x)

  if (is.null(n)) n <- length(x[["id_str"]])

  for (i in toplevel) {
    if (!i %in% names(x)) {

      if (i %in% c("id_str", "name", "screen_name", "location",
        "description", "url", "created_at", "favourites_count",
        "utc_offset", "time_zone", "lang")) {
        x[[i]] <- rep(NA_character_, n)
      } else if (i %in% c("followers_count", "friends_count",
        "listed_count", "favourites_count", "favorite_count",
        "statuses_count")) {
        x[[i]] <- rep(NA_integer_, n)
      } else if (i == c("protected", "geo_enabled", "verified")) {
        x[[i]] <- rep(NA, n)
      } else {
        x[[i]] <- rep(NA, n)
      }

    }
  }

  toplevel_df <- lapply(x[toplevel], return_with_NA)

  names(toplevel_df) <- gsub("_str", "", names(toplevel_df))

  names(toplevel_df)[names(toplevel_df) == "id"] <- "user_id"

  if ("created_at" %in% names(toplevel_df)) {
    toplevel_df[["created_at"]] <- format_date(
      toplevel_df[["created_at"]], date = FALSE)
  }

  data_frame_(toplevel_df)
}

data_frame_ <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}
as_data_frame_ <- function(...) {
  as.data.frame(..., stringsAsFactors = FALSE)
}
rbindr_ <- function(...) {
  rbind(...)
}
rbind_ <- function(...) {
  do.call("rbindr_", ...)
}
cbind_ <- function(...) {
  cbind(..., stringsAsFactors = FALSE)
}

user_entities_df <- function(dat, n = NULL) {

  dat <- check_user_obj(dat)

  if (is.null(n)) n <- length(dat[["id_str"]])

  user_ent_df <- data_frame_(
    url = rep(NA_character_, n),
    description_urls = rep(NA_character_, n))

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





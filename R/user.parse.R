
#' usr_ent_urls
#' @export
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

#' user_entities_df
#' @export
user_entities_df <- function(x, n = NULL) {

  x <- check_response_obj(x)

  if (is.null(n)) n <- length(x[["id_str"]])

  user_ent_df <- dplyr::data_frame(
    user_url = rep(NA_character_, n),
    user_description_urls = as.list(rep(NA_character_, n)))

  if ("user" %in% names(x)) {
    user <- x[["user"]]

    if ("entities" %in% names(user)) {
      entities <- user[["entities"]]

      if ("url" %in% names(entities)) {
        ent_url <- entities[["url"]]

        if ("urls" %in% names(ent_url)) {
          user_ent_df$user_url <- unlist(lapply(
            ent_url[["urls"]], usr_ent_urls))
        }
      }

      if ("description" %in% names(entities)) {
        ent_url <- entities[["description"]]

        if ("urls" %in% names(ent_url)) {
          user_ent_df$user_description_urls <- lapply(
            ent_url[["urls"]], usr_ent_urls)
        }
      }
    }
  }
  user_ent_df
}

#' .make_user
#' @export
.make_user <- function(x, n = NULL) {
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

  toplevel <- c(
    "id_str", "name", "screen_name", "location", "description",
    "url", "protected", "followers_count", "friends_count", "listed_count",
    "created_at", "favourites_count", "utc_offset", "time_zone",
    "geo_enabled", "verified", "statuses_count", "lang")

  x <- check_response_obj(x)

  if (is.null(n)) n <- length(x[["id_str"]])

  toplevel_df <- lapply(x[toplevel], return_with_NA)

  if ("created_at" %in% names(toplevel_df)) {
    toplevel_df[["created_at"]] <- format_date(
      toplevel_df[["created_at"]], date = FALSE)
  }

  .extend_label_df(user_df, "user")

  dplyr::tbl_df(toplevel_df)

  if (is.null(n)) n <- length(dat[["id_str"]])

  if (!"user" %in% names(dat)) {

  } else {
    user <- dat[["user"]]
    user_df <- lapply(toplevel, function(i) return_with_NA(user[[i]], n))
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

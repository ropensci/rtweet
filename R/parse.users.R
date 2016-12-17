parse_users <- function(x, as_double = FALSE) {
    if (!is.recursive(x)) return(data.frame())
    if ("friends_count" %in% names(x)) {
        return(user_df(x, as_double = as_double))
    }
    if ("statuses" %in% names(x)) {
        x <- x[["statuses"]]
        return(user_df(x, as_double = as_double))
    }
    if ("user" %in% names(x)) {
        return(user_df(x[["user"]], as_double = as_double))
    }
    invisible()
}

user_df <- function(dat, as_double = FALSE) {
    if ("user" %in% names(dat)) {
        dat <- dat[["user"]]
    }
    user_df <- cbind(
        user_toplevel_df(dat, as_double = as_double),
        user_entities_df(dat))
    unique(user_df)
}

usr_ent_urls <- function(x, list = FALSE) {
    if (is.data.frame(x)) {
        if ("expanded_url" %in% names(x)) {
            x <- x[["expanded_url"]]
        } else {
            x <- NA_character_
        }
    } else {
        x <- NA_character_
    }
    x
}

user_toplevel_df <- function(x, n = NULL, names = NULL,
                             add.names = NULL,
                             as_double = FALSE) {

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
                         "description", "url", "created_at",
                         "utc_offset", "time_zone", "lang")) {
                x[[i]] <- rep(NA_character_, n)
            } else if (i %in% c("followers_count", "friends_count",
                                "listed_count", "favourites_count",
                                "favorite_count",
                                "statuses_count")) {
                x[[i]] <- rep(NA_integer_, n)
            } else if (i %in% c("protected", "geo_enabled", "verified")) {
                x[[i]] <- rep(NA, n)
            } else {
                x[[i]] <- rep(NA, n)
            }
        }
    }

    toplevel_df <- lapply(x[names(x) %in% toplevel], return_with_NA)
    names(toplevel_df) <- gsub("_str", "", names(toplevel_df))
    names(toplevel_df)[names(toplevel_df) == "id"] <- "user_id"

    if ("created_at" %in% names(toplevel_df)) {
        toplevel_df[["created_at"]] <- format_date(
            toplevel_df[["created_at"]], date = FALSE)
    }
    if (as_double) {
        toplevel_df[["user_id"]] <- as.double(
            toplevel_df[["user_id"]])
    } else {
        toplevel_df[["user_id"]] <- as.character(
            toplevel_df[["user_id"]])
    }
    data.frame(toplevel_df, stringsAsFactors = FALSE)
}

user_entities_df <- function(dat, n = NULL) {

    dat <- check_user_obj(dat)

    if (is.null(n)) n <- length(dat[["id_str"]])

    user_ent_df <- data.frame(
        ##url = rep(NA_character_, n),
        description_urls = rep(NA_character_, n),
  	stringsAsFactors = FALSE)

    if ("entities" %in% names(dat)) {
        entities <- dat[["entities"]]

        if ("url" %in% names(entities)) {
            ent_url <- entities[["url"]]

            ##if ("urls" %in% names(ent_url)) {
            ##  user_ent_df$url <- flatten(ent_url[["urls"]])
            ##  }
        }

        if ("description" %in% names(entities)) {
            ent_url <- entities[["description"]]

            if ("urls" %in% names(ent_url)) {
                user_ent_df$description_urls <- flatten(ent_url[["urls"]])
            }
        }
    }
    user_ent_df
}





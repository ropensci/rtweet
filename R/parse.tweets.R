parse_tweets <- function(x, clean_tweets = FALSE,
  as_double = FALSE) {
  if ("statuses" %in% names(x)) {
    x <- x[["statuses"]]
  } else if ("status" %in% names(x)) {
    x <- x[["status"]]
  }

  if (!"friends_count" %in% names(x)) {
    x <- tweets_df(x, clean_tweets = clean_tweets,
      as_double = as_double)
    return(x)
  }

  return(invisible())
}

tweets_df <- function(dat, as_double = FALSE, clean_tweets = FALSE) {

  if (missing(dat)) {
    stop("Must specify tweets object, dat.", call. = TRUE)
  }

  if ("statuses" %in% names(dat)) {
    dat <- dat[["statuses"]]
  }
  if ("status" %in% names(dat)) {
    dat <- dat[["status"]]
  }

  tweets_df <- cbind(
    tweets_toplevel_df(dat, as_double = as_double),
    tweets_entities_df(dat),
    tweets_retweet_df(dat),
    tweets_place_df(dat))

  if (clean_tweets) {
    tweets_df[["text"]] <- cleantweets(tweets_df[["text"]])
  }
  tweets_df <- tweets_df[row.names(unique(tweets_df[, 1:13])), ]
  row.names(tweets_df) <- NULL
  tweets_df
}

cleantweets <- function(x) {
  iconv(x, "UTF-8", "ASCII", "")
}

tweets_toplevel_df <- function(dat, n = NULL, names = NULL,
                               add.names = NULL,
                               as_double = FALSE) {

  if (missing(dat)) {
    stop("Must specify tweets object, dat.", call. = TRUE)
  }

  if (is.null(names)) {
    toplevel <- c("created_at", "id_str", "retweet_count",
      "favorite_count", "text", "in_reply_to_status_id_str",
      "in_reply_to_user_id_str", "in_reply_to_screen_name",
      "is_quote_status", "quoted_status_id_str",
      "source", "lang")
  }

  if (!is.null(add.names)) {
    toplevel <- c(toplevel, add.names)
  }

  dat <- check_response_obj(dat)

  if (is.null(n)) n <- length(dat[["id_str"]])
  toplevel_df <- data.frame()
  toplevel_df <- data.frame(lapply(dat[names(dat) %in% toplevel],
  	return_with_NA), stringsAsFactors = FALSE)

  names(toplevel_df) <- gsub("_str", "", names(toplevel_df))

  names(toplevel_df)[names(toplevel_df) == "id"] <- "status_id"

  if (as_double) {
    toplevel_df$user_id <- as.double(check_user_id(dat))
  } else {
    toplevel_df$user_id <- as.character(check_user_id(dat))
  }

  toplevel_df$screen_name <- check_screen_name(dat)

  if ("created_at" %in% names(toplevel_df)) {
    toplevel_df[["created_at"]] <- format_date(
      toplevel_df[["created_at"]], date = FALSE)
  }
  if ("source" %in% names(toplevel_df)) {
    toplevel_df[["source"]] <- unlist(lapply(
      strsplit(as.character(toplevel_df[["source"]]), "[<]|[>]"),
      function(x) x[3]), use.names = FALSE)
  }

  toplevel <- gsub("_str", "", toplevel)
  toplevel <- toplevel[!toplevel %in% c(names(toplevel_df), "id")]

  if (!identical(length(toplevel), 0L)) {
  	for (i in toplevel) {
  		if (i == "created_at") {
  			toplevel_df[["created_at"]] <- format_date(rep(NA_character_, n))
  		} else if (i == "id") {
  			toplevel_df[["status_id"]] <- rep(NA_real_, n)
  		} else if (i == "in_reply_to_status_id") {
  			toplevel_df[["in_reply_to_status_id"]] <- rep(NA_character_, n)
  		} else if (i == "text") {
  			toplevel_df[["text"]] <- rep(NA_character_, n)
  		} else if (i == "in_reply_to_user_id") {
  			toplevel_df[["in_reply_to_user_id"]] <- rep(NA_character_, n)
  		} else if (i == "in_reply_to_screen_name") {
  			toplevel_df[["in_reply_to_screen_name"]] <- rep(NA_character_, n)
  		} else if (i == "lang") {
  			toplevel_df[["lang"]] <- rep(NA_character_, n)
  		} else if (i == "retweet_count") {
  			toplevel_df[["retweet_count"]] <- rep(NA_integer_, n)
  		} else if (i == "favorite_count") {
  			toplevel_df[["favorite_count"]] <- rep(NA_integer_, n)
  		} else if (i == "is_quote_status") {
  			toplevel_df[["is_quote_status"]] <- rep(NA, n)
  		} else if (i == "quoted_status_id") {
  			toplevel_df[["quoted_status_id"]] <- rep(NA_real_, n)
  		}
  	}
  }

  if (as_double) {
    toplevel_df[["status_id"]] <- as.double(
      toplevel_df[["status_id"]])
    if ("user_id" %in% names(toplevel_df)) {
      toplevel_df[["user_id"]] <- as.double(
        toplevel_df[["user_id"]])
    }
    toplevel_df[["quoted_status_id"]] <- as.double(
      toplevel_df[["quoted_status_id"]])
    toplevel_df[["in_reply_to_status_id"]] <- as.double(
      toplevel_df[["in_reply_to_status_id"]])
    toplevel_df[["in_reply_to_user_id"]] <- as.double(
      toplevel_df[["in_reply_to_user_id"]])
  } else {
    toplevel_df[["status_id"]] <- as.character(
      toplevel_df[["status_id"]])
    if ("user_id" %in% names(toplevel_df)) {
      toplevel_df[["user_id"]] <- as.character(
        toplevel_df[["user_id"]])
    }
    toplevel_df[["quoted_status_id"]] <- as.character(
      toplevel_df[["quoted_status_id"]])
    toplevel_df[["in_reply_to_status_id"]] <- as.character(
      toplevel_df[["in_reply_to_status_id"]])
    toplevel_df[["in_reply_to_user_id"]] <- as.character(
      toplevel_df[["in_reply_to_user_id"]])
  }
  toplevel_df
}

tweets_entities_df <- function(dat, n = NULL) {

  if (missing(dat)) {
    stop("Must specify tweets object, dat.", call. = TRUE)
  }

  dat <- check_response_obj(dat)

  if (is.null(n)) n <- length(dat[["id_str"]])

  ent_df <- data.frame(
    mentions_user_id = rep(NA_real_, n),
  	mentions_screen_name = rep(NA_character_, n),
    hashtags = rep(NA_character_, n),
    entitites_urls = rep(NA_character_, n), stringsAsFactors = FALSE)

  if ("entities" %in% names(dat)) {
    entities <- dat[["entities"]]

    if ("user_mentions" %in% names(entities)) {
      ent_df$mentions_user_id <- flatten(lapply(entities[["user_mentions"]],
        function(x) return_with_NA(x[["id_str"]], 1)))
    }
    if ("user_mentions" %in% names(entities)) {
    	ent_df$mentions_screen_name <- flatten(lapply(
        entities[["user_mentions"]],
    		function(x) return_with_NA(x[["screen_name"]], 1)))
    }

    if ("hashtags" %in% names(entities)) {
      ent_df$hashtags <- flatten(lapply(entities[["hashtags"]],
        function(x) return_with_NA(x[["text"]], 1)))
    }

    if ("urls" %in% names(entities)) {
      ent_df$entitites_urls <- flatten(lapply(entities[["urls"]],
        function(x) return_with_NA(x[["expanded_url"]], 1)))
    }
  }
  ent_df
}

tweets_retweet_df <- function(dat, n = NULL) {

  if (missing(dat)) {
    stop("Must specify tweets object, dat.", call. = TRUE)
  }

  dat <- check_response_obj(dat)

  if (is.null(n)) n <- length(dat[["id_str"]])

  retweet_df <- data.frame(
    is_retweet = rep(NA, n),
    retweet_status_id = rep(NA_character_, n),
  	stringsAsFactors = FALSE)

  retweeted_status <- dat[["retweeted_status"]]

  retweet_df$is_retweet <- !is.na(return_with_NA(
    retweeted_status[["id_str"]], n))

  retweet_df$retweet_status_id <- return_with_NA(
    retweeted_status[["id_str"]], n)

  retweet_df
}

make_coords <- function(x) {
  if (is.array(x)) {
    coords <- matrix(as.numeric(x), 1, 8)
  } else {
    coords <- matrix(NA_real_, 1, 8)
  }
  coords
}


tweets_place_df <- function(dat, n = NULL) {

  if (missing(dat)) {
    stop("Must specify tweets object, dat.", call. = TRUE)
  }

  dat <- check_response_obj(dat)

  if (is.null(n)) n <- length(dat[["id_str"]])

  place_df <- data.frame(
    place_name = rep(NA_character_, n),
    country = rep(NA_character_, n),
    coordinates = I(matrix(rep(matrix(NA_real_, 1, 8), n), ncol = 8)),
  	stringsAsFactors = FALSE)

  if ("place" %in% names(dat)) {
    place <- dat[["place"]]

    if ("full_name" %in% names(place)) {
      place_df$place_name <- return_with_NA(place[["full_name"]], n)
    }
    if ("country" %in% names(place)) {
      place_df$country <- return_with_NA(place[["country"]], n)
    }

    if ("bounding_box" %in% names(place)) {
      bounding_box <- place[["bounding_box"]]

      if ("coordinates" %in% names(bounding_box)) {
        coordinates <- bounding_box[["coordinates"]]
        place_df$coordinates <- t(sapply(coordinates, make_coords))
      }
    }
  }
  place_df
}

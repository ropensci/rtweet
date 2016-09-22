tweets_toplevel_df <- function(dat, n = NULL, names = NULL,
                               add.names = NULL,
                               clean.tweets = FALSE) {

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

  clean_tweets <- clean.tweets

  dat <- check_response_obj(dat)

  if (is.null(n)) n <- length(dat[["id_str"]])

  for (i in toplevel) {
    if (!i %in% names(dat)) {

      if (i %in% c("created_at", "id_str", "in_reply_to_status_id_str",
      	"in_reply_to_user_id_str", "quoted_status_id_str",
        "text", "in_reply_to_screen_name", "lang")) {
        dat[[i]] <- rep(NA_character_, n)
      } else if (i %in% c("retweet_count", "favorite_count")) {
        dat[[i]] <- rep(NA_integer_, n)
      } else if (i == "is_quote_status") {
        dat[[i]] <- rep(NA, n)
      #} else if (i == c("id_str", "in_reply_to_status_id_str",
      	#"in_reply_to_user_id_str", "quoted_status_id_str")) {
      	#dat[[i]] <- rep(NA_real_, n)
      } else {
        dat[[i]] <- rep(NA, n)
      }

    }
  }
  if (clean_tweets) {
    dat[["text"]] <- clean_tweets(dat[["text"]])
  }

  toplevel_df <- lapply(dat[toplevel], return_with_NA)

  toplevel_df$user_id <- check_user_id(dat)
  toplevel_df$screen_name <- check_screen_name(dat)

  names(toplevel_df) <- gsub("_str", "", names(toplevel_df))

  names(toplevel_df)[names(toplevel_df) == "id"] <- "status_id"

  if ("created_at" %in% names(toplevel_df)) {
    toplevel_df[["created_at"]] <- format_date(
      toplevel_df[["created_at"]], date = FALSE)
  }
  if ("source" %in% names(toplevel_df)) {
    toplevel_df[["source"]] <- sapply(
      strsplit(as.character(toplevel_df[["source"]]), "[<]|[>]"),
      function(x) x[3])
  }
  toplevel_df[["status_id"]] <- as.double(
  	toplevel_df[["status_id"]])
  toplevel_df[["user_id"]] <- as.double(
  	toplevel_df[["user_id"]])
  toplevel_df[["quoted_status_id"]] <- as.double(
  	toplevel_df[["quoted_status_id"]])
  toplevel_df[["in_reply_to_status_id"]] <- as.double(
  	toplevel_df[["in_reply_to_status_id"]])
  toplevel_df[["in_reply_to_user_id"]] <- as.double(
  	toplevel_df[["in_reply_to_user_id"]])

  data_frame_(toplevel_df)
}

tweets_entities_df <- function(dat, n = NULL) {

  if (missing(dat)) {
    stop("Must specify tweets object, dat.", call. = TRUE)
  }

  dat <- check_response_obj(dat)

  if (is.null(n)) n <- length(dat[["id_str"]])

  ent_df <- data_frame_(
    mentions_user_id = I(as.list(rep(NA_real_, n))),
  	mentions_screen_name = I(as.list(rep(NA_character_, n))),
    hashtags = I(as.list(rep(NA_character_, n))),
    urls = I(as.list(rep(NA_character_, n))))

  if ("entities" %in% names(dat)) {
    entities <- dat[["entities"]]

    if ("user_mentions" %in% names(entities)) {
      ent_df$mentions_user_id <- lapply(entities[["user_mentions"]],
        function(x) return_with_NA(x[["id_str"]], 1))
      ent_df$mentions_user_id <- lapply(ent_df$mentions_user_id,
      	as.double)
    }
    if ("user_mentions" %in% names(entities)) {
    	ent_df$mentions_screen_name <- lapply(entities[["user_mentions"]],
    		function(x) return_with_NA(x[["screen_name"]], 1))
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

tweets_retweet_df <- function(dat, n = NULL) {

  if (missing(dat)) {
    stop("Must specify tweets object, dat.", call. = TRUE)
  }

  dat <- check_response_obj(dat)

  if (is.null(n)) n <- length(dat[["id_str"]])

  retweet_df <- data_frame_(
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

make_coords <- function(x) {

  if (is.array(x)) {
    coords <- matrix(as.numeric(x), 1, 8)
    #names(coords_df) <- c(
    #  "long1", "long2", "long3", "long4",
    #  "lat1", "lat2", "lat3", "lat4")
  } else {
    coords <- matrix(rep(NA_real_, 8), 1, 8)

    #names(coords_df) <- c(
    #  "long1", "long2", "long3", "long4",
    #  "lat1", "lat2", "lat3", "lat4")
  }

  coords
}


tweets_place_df <- function(dat, n = NULL) {

  if (missing(dat)) {
    stop("Must specify tweets object, dat.", call. = TRUE)
  }

  dat <- check_response_obj(dat)

  if (is.null(n)) n <- length(dat[["id_str"]])

  place_df <- data_frame_(
    place_name = rep(NA_character_, n),
    country = rep(NA_character_, n),
    coordinates = rep(NA_character_, n))
    #long1 = rep(NA_real_, n),
    #long2 = rep(NA_real_, n),
    #long3 = rep(NA_real_, n),
    #long4 = rep(NA_real_, n),
    #lat1 = rep(NA_real_, n),
    #lat2 = rep(NA_real_, n),
    #lat3 = rep(NA_real_, n),
    #lat4 = rep(NA_real_, n))

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
        coordinates <- lapply(coordinates, make_coords)

        for (i in seq_along(coordinates)) {
          place_df$coordinates <- coordinates
        }
      }
    }
  }

  place_df
}

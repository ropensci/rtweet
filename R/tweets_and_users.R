#' Parsing data into tweets/users data tibbles
#' 
#' For internal use only
#' 
#' @param x A list of responses, with one element for each page. 
#' @return A tweets/users tibble with users/tweets attribute.
#' @keywords internal
#' @export
tweets_with_users <- function(x) {
  tweets <- do.call("rbind", lapply(x, tweet))
  users <- tweets$user
  tweets <- tweets[!colnames(tweets) %in% "user"]

  structure(tweets, users = users)
}

#' @rdname tweets_with_users
#' @export
users_with_tweets <- function(x) {
  users <- do.call("rbind", lapply(x, user))
  tweets <- do.call("rbind", lapply(x[["status"]], tweet))
  
  structure(users, tweets = tweets)
}

##-----------------------------------------------------
## safe extractors
##-----------------------------------------------------
`[[[` <- function(dat, var, NA_ = NA_character_) {
  if (length(dat) == 0L) {
    return(NA_)
  } else if (!is.recursive(dat)) {
    dat[lengths(dat) == 0L] <- NA_
    return(dat)
  }
  x <- `[[`(dat, var)
  ## if empty give number of NAs equal to obs
  if (length(x) == 0L && is.data.frame(dat)) {
    return(rep(NA_, length.out = nrow(dat)))
  } else if (length(x) == 0L && is.list(dat)) {
    return(rep(NA_, length.out = length(dat)))
  } else if (length(x) == 0L) {
    return(NA_)
  }
  x[lengths(x) == 0L] <- NA_
  x
}


tweets_to_tbl_ <- function(dat) {
  if (NROW(dat) == 0L) return(data.frame())
  dat$display_text_width <- display_text_range(dat)
  ## extended entities > media
  if (has_name_children(dat, "extended_entities", "media")) {
    media <- dat$extended_entities$media
    dat$ext_media_url <- lapply(media, "[[[", "media_url")
    dat$ext_media_expanded_url <- lapply(media, "[[[", "expanded_url")
    dat$ext_media_t.co <- lapply(media, "[[[", "url")
    dat$ext_media_type <- lapply(media, "[[[", "type")
    dat$ext_alt_text <- lapply(media, "[[[", "ext_alt_text")
  } else {
    dat$ext_media_url <- as.list(NA_character_)
    dat$ext_media_expanded_url <- as.list(NA_character_)
    dat$ext_media_t.co <- as.list(NA_character_)
    dat$ext_media_type <- as.list(NA_character_)
    dat$ext_alt_text <- as.list(NA_character_)
  }
  ## entities > urls
  if (has_name_children(dat, "entities", "urls")) {
    dat$urls_url <- lapply(dat$entities$urls, "[[[", "display_url")
    dat$urls_expanded_url <- lapply(dat$entities$urls, "[[[", "expanded_url")
    dat$urls_t.co <- lapply(dat$entities$urls, "[[[", "url")
  } else {
    dat$urls_url <- as.list(NA_character_)
    dat$urls_expanded_url <- as.list(NA_character_)
    dat$urls_t.co <- as.list(NA_character_)
  }
  ## entities > media
  if (has_name_children(dat, "entities", "media")) {
    dat$media_url <- lapply(dat$entities$media, "[[[", "media_url")
    dat$media_expanded_url <- lapply(dat$entities$media, "[[[", "expanded_url")
    dat$media_t.co <- lapply(dat$entities$media, "[[[", "url")
    dat$media_type <- lapply(dat$entities$media, "[[[", "type")
  } else {
    dat$media_url <- as.list(NA_character_)
    dat$media_expanded_url <- as.list(NA_character_)
    dat$media_t.co <- as.list(NA_character_)
    dat$media_type <- as.list(NA_character_)
  }
  ## entities > user_mentions
  if (has_name_children(dat, "entities", "user_mentions")) {
    dat$mentions_user_id <- lapply(
      dat$entities$user_mentions, "[[[", "id_str")
    dat$mentions_screen_name <- lapply(
      dat$entities$user_mentions, "[[[", "screen_name")
  } else {
    dat$mentions_user_id <- as.list(NA_character_)
    dat$mentions_screen_name <- as.list(NA_character_)
  }
  ## entities > hashtags
  if (has_name_children(dat, "entities", "hashtags")) {
    dat$hashtags <- lapply(dat$entities$hashtags, "[[[", "text")
  } else {
    dat$hashtags <- as.list(NA_character_)
  }
  if (has_name_children(dat, "entities", "symbols")) {
    dat$symbols <- lapply(dat$entities$symbols, "[[[", "text")
  } else {
    dat$symbols <- as.list(NA_character_)
  }
  if (has_name_children(dat, "geo", "coordinates")) {
    dat$geo_coords <- lapply(
      dat$geo$coordinates, `[[[`, 1, NA_ = c(NA_real_, NA_real_))
  } else {
    dat$geo_coords <- list(c(NA_real_, NA_real_))
  }
  if (has_name_children(dat, "coordinates", "coordinates")) {
    dat$coordinates_coords <- lapply(
      dat$coordinates$coordinates, `[[[`, 1, NA_ = c(NA_real_, NA_real_))
  } else {
    dat$coordinates_coords <- list(c(NA_real_, NA_real_))
  }
  if (has_name_children(dat, "place", "id")) {
    dat$place_url <- `[[[`(dat$place, "url")
    dat$place_full_name <- `[[[`(dat$place, "full_name")
    dat$place_name <- `[[[`(dat$place, "name")
    dat$country_code <- `[[[`(dat$place, "country_code")
    dat$place_type <- `[[[`(dat$place, "place_type")
    dat$country <- `[[[`(dat$place, "country")
    if (has_name_children(dat$place, "bounding_box", "coordinates")) {
      dat$bbox_coords <- lapply(
        dat$place$bounding_box[["coordinates"]], function(i) {
          if (is.array(i)) {
            c(i[1, , 1], i[1, , 2])
          } else {
            c(NA_real_, NA_real_, NA_real_, NA_real_,
              NA_real_, NA_real_, NA_real_, NA_real_)
          }
        })

    } else {
      dat$bbox_coords <- list(
        c(NA_real_, NA_real_, NA_real_, NA_real_,
          NA_real_, NA_real_, NA_real_, NA_real_)
      )
    }
  } else {
    dat$place_url <- NA_character_
    dat$place_full_name <- NA_character_
    dat$place_name <- NA_character_
    dat$country_code <- NA_character_
    dat$place_type <- NA_character_
    dat$country <- NA_character_
    dat$bbox_coords <- list(
        c(NA_real_, NA_real_, NA_real_, NA_real_,
          NA_real_, NA_real_, NA_real_, NA_real_)
      )
  }
  if (has_name_children(dat, "user", "id_str")) {
    dat$user_id <- `[[[`(dat$user, "id_str")
    dat$screen_name <- `[[[`(dat$user, "screen_name")
  } else if (has_name_(dat, "screen_name") && has_name_(dat, "id_str")) {
    dat$user_id <- dat[["id_str"]]
    dat$id_str <- NULL
    
  } else {
    dat$user_id <- NA_character_
    dat$screen_name <- NA_character_
  }
  ## full text
  if (has_name_(dat, "full_text")) {
    dat$text <- dat$full_text
  }
  if (has_name_children(dat, "extended_tweet", "full_text")) {
    is_et <- !is.na(dat$extended_tweet$full_text)
    dat$text[is_et] <- dat$extended_tweet$full_text[is_et]
  }
  dat <- wrangle_quote_status(dat)
  dat <- wrangle_retweet_status(dat)
  statuscols <- statuscols_()
  nacols <- statuscols[!statuscols %in% names(dat)]
  for (i in nacols) {
    if (grepl("_count$", i)) {
      dat[[i]] <- NA_integer_
    } else if (grepl("^is_", i)) {
      dat[[i]] <- NA
    } else {
      dat[[i]] <- NA_character_
    }
  }
  dat <- dat[, statuscols[statuscols %in% names(dat)]]
  names(dat) <- names(statuscols)[statuscols %in% names(dat)]
  dat$created_at <- format_date(dat$created_at)
  dat$source <- clean_source_(dat$source)
  dat <- status_url_(dat)
  as_tbl(dat)
}

as_tbl <- function(x, ...) {
  tibble::as_tibble(x, ...)
}

statuscols_ <- function() {
  c(
    status_id = "id_str",
    created_at = "created_at",
    user_id = "user_id",
    screen_name = "screen_name",
    text = "text",
    source = "source",
    display_text_width = "display_text_width",
    reply_to_status_id = "in_reply_to_status_id_str",
    reply_to_user_id = "in_reply_to_user_id_str",
    reply_to_screen_name = "in_reply_to_screen_name",
    is_quote = "is_quote",
    is_retweet = "is_retweet",
    favorite_count = "favorite_count",
    retweet_count = "retweet_count",
    quote_count = "quote_count",
    reply_count = "reply_count",
    hashtags = "hashtags",
    symbols = "symbols",
    urls_url = "urls_url",
    urls_t.co = "urls_t.co",
    urls_expanded_url = "urls_expanded_url",
    media_url = "media_url",
    media_t.co = "media_t.co",
    media_expanded_url = "media_expanded_url",
    media_type = "media_type",
    ext_media_url = "ext_media_url",
    ext_media_t.co = "ext_media_t.co",
    ext_media_expanded_url = "ext_media_expanded_url",
    ext_media_type = "ext_media_expanded_type",
    ext_alt_text = "ext_alt_text",
    mentions_user_id = "mentions_user_id",
    mentions_screen_name = "mentions_screen_name",
    lang = "lang",
    quoted_status_id = "quoted_status_id",
    quoted_text = "quoted_text",
    quoted_created_at = "quoted_created_at",
    quoted_source = "quoted_source",
    quoted_favorite_count = "quoted_favorite_count",
    quoted_retweet_count = "quoted_retweet_count",
    quoted_user_id = "quoted_user_id",
    quoted_screen_name = "quoted_screen_name",
    quoted_name = "quoted_name",
    quoted_followers_count = "quoted_followers_count",
    quoted_friends_count = "quoted_friends_count",
    quoted_statuses_count = "quoted_statuses_count",
    quoted_location = "quoted_location",
    quoted_description = "quoted_description",
    quoted_verified = "quoted_verified",
    retweet_status_id = "retweet_status_id",
    retweet_text = "retweet_text",
    retweet_created_at = "retweet_created_at",
    retweet_source = "retweet_source",
    retweet_favorite_count = "retweet_favorite_count",
    retweet_retweet_count = "retweet_retweet_count",
    retweet_user_id = "retweet_user_id",
    retweet_screen_name = "retweet_screen_name",
    retweet_name = "retweet_name",
    retweet_followers_count = "retweet_followers_count",
    retweet_friends_count = "retweet_friends_count",
    retweet_statuses_count = "retweet_statuses_count",
    retweet_location = "retweet_location",
    retweet_description = "retweet_description",
    retweet_verified = "retweet_verified",
    place_url = "place_url",
    place_name = "place_name",
    place_full_name = "place_full_name",
    place_type = "place_type",
    country = "country",
    country_code = "country_code",
    geo_coords = "geo_coords",
    coords_coords = "coordinates_coords",
    bbox_coords = "bbox_coords"
  )
}

display_text_range <- function(x) {
  if (has_name_(x, "display_text_range")) {
    vapply(
      x$display_text_range,
      function(x) ifelse(is.null(x), NA_integer_, diff(x)), double(1))
  } else {
    rep(NA_integer_, nrow(x))
  }
}


##-----------------------------------------------------
## utility funs
##-----------------------------------------------------

clean_source_ <- function(s) {
  sub("<\\/.*", "", sub("[^>]+>", "", s))
}


wrangle_retweet_status <- function(x) {
  n <- nrow(x)
  if (has_name_(x, "retweeted_status")) {
    rst <- x[["retweeted_status"]]
  } else {
    rst <- data.frame()
  }
  ## user mentions
  if (has_name_(rst, "id_str")) {
    x$retweet_status_id <- rst$id_str
  } else {
    x$retweet_status_id <- NA_character_
  }
  if (has_name_(rst, "retweet_count")) {
    x$retweet_retweet_count <- rst$retweet_count
  } else {
    x$retweet_retweet_count <- NA_integer_
  }
  if (has_name_(rst, "full_text")) {
    x$retweet_text <- rst$full_text
  } else if (has_name_(rst, "text")) {
    x$retweet_text <- rst$text
  } else {
    x$retweet_text <- NA_character_
  }
  if (has_name_children(rst, "extended_tweet", "full_text")) {
    is_et <- !is.na(rst$extended_tweet$full_text)
    x$retweet_text[is_et] <- rst$extended_tweet$full_text[is_et]
  }
  ## make 'text' include full retweet text
  rt_et <- !is.na(x$retweet_text)
  x$text[rt_et] <- x$retweet_text[rt_et]
  if (has_name_(rst, "source")) {
    x$retweet_source <- clean_source_(rst$source)
  } else {
    x$retweet_source <- NA_character_
  }
  if (has_name_(rst, "created_at")) {
    x$retweet_created_at <- format_date(rst$created_at)
  } else {
    x$retweet_created_at <- as.POSIXct(NA_character_)
  }
  if (has_name_(rst, "favorite_count")) {
    x$retweet_favorite_count <- rst$favorite_count
  } else {
    x$retweet_favorite_count <- NA_integer_
  }
  if (has_name_children(rst, "user", "screen_name")) {
    x$retweet_screen_name <- rst$user$screen_name
  } else {
    x$retweet_screen_name <- NA_character_
  }
  if (has_name_children(rst, "user", "id_str")) {
    x$retweet_user_id <- rst$user$id_str
  } else {
    x$retweet_user_id <- NA_character_
  }
  ##
  if (has_name_children(rst, "user", "name")) {
    x$retweet_name <- rst$user$name
  } else {
    x$retweet_name <- NA_character_
  }
  if (has_name_children(rst, "user", "description")) {
    x$retweet_description <- rst$user$description
  } else {
    x$retweet_description <- NA_character_
  }
  if (has_name_children(rst, "user", "followers_count")) {
    x$retweet_followers_count <- rst$user$followers_count
  } else {
    x$retweet_followers_count <- NA_integer_
  }
  if (has_name_children(rst, "user", "friends_count")) {
    x$retweet_friends_count <- rst$user$friends_count
  } else {
    x$retweet_friends_count <- NA_integer_
  }
  if (has_name_children(rst, "user", "statuses_count")) {
    x$retweet_statuses_count <- rst$user$statuses_count
  } else {
    x$retweet_statuses_count <- NA_integer_
  }
  if (has_name_children(rst, "user", "verified")) {
    x$retweet_verified <- rst$user$verified
  } else {
    x$retweet_verified <- NA
  }
  if (has_name_children(rst, "user", "location")) {
    x$retweet_location <- rst$user$location
  } else {
    x$retweet_location <- NA_character_
  }
  x$is_retweet <- !is.na(x$retweet_status_id)
  x[["retweeted_status"]] <- NULL
  x
}

wrangle_quote_status <- function(x) {
  n <- nrow(x)
  if (has_name_(x, "quoted_status")) {
    qst <- x[["quoted_status"]]
  } else {
    qst <- data.frame()
  }
  ## user mentions
  if (has_name_(qst, "id_str")) {
    x$quoted_status_id <- qst$id_str
  } else {
    x$quoted_status_id <- NA_character_
  }
  if (has_name_(qst, "full_text")) {
    x$quoted_text <- qst$full_text
  } else if (has_name_(qst, "text")) {
    x$quoted_text <- qst$text
  } else {
    x$quoted_text <- NA_character_
  }
  if (has_name_children(qst, "extended_tweet", "full_text")) {
    is_et <- !is.na(qst$extended_tweet$full_text)
    x$quoted_text[is_et] <- qst$extended_tweet$full_text[is_et]
  }
  if (has_name_(qst, "source")) {
    x$quoted_source <- clean_source_(qst$source)
  } else {
    x$quoted_source <- NA_character_
  }
  if (has_name_(qst, "created_at")) {
    x$quoted_created_at <- format_date(qst$created_at)
  } else {
    x$quoted_created_at <- as.POSIXct(NA_character_)
  }
  if (has_name_(qst, "favorite_count")) {
    x$quoted_favorite_count <- qst$favorite_count
  } else {
    x$quoted_favorite_count <- NA_integer_
  }
  if (has_name_(qst, "created_at")) {
    x$quoted_retweet_count <- qst$retweet_count
  } else {
    x$quoted_retweet_count <- NA_integer_
  }
  if (has_name_children(qst, "user", "screen_name")) {
    x$quoted_screen_name <- qst$user$screen_name
  } else {
    x$quoted_screen_name <- NA_character_
  }
  if (has_name_children(qst, "user", "id_str")) {
    x$quoted_user_id <- qst$user$id_str
  } else {
    x$quoted_user_id <- NA_character_
  }
  if (has_name_children(qst, "user", "name")) {
    x$quoted_name <- qst$user$name
  } else {
    x$quoted_name <- NA_character_
  }
  if (has_name_children(qst, "user", "description")) {
    x$quoted_description <- qst$user$description
  } else {
    x$quoted_description <- NA_character_
  }
  if (has_name_children(qst, "user", "followers_count")) {
    x$quoted_followers_count <- qst$user$followers_count
  } else {
    x$quoted_followers_count <- NA_integer_
  }
  if (has_name_children(qst, "user", "friends_count")) {
    x$quoted_friends_count <- qst$user$friends_count
  } else {
    x$quoted_friends_count <- NA_integer_
  }
  if (has_name_children(qst, "user", "statuses_count")) {
    x$quoted_statuses_count <- qst$user$statuses_count
  } else {
    x$quoted_statuses_count <- NA_integer_
  }
  if (has_name_children(qst, "user", "verified")) {
    x$quoted_verified <- qst$user$verified
  } else {
    x$quoted_verified <- NA
  }
  if (has_name_children(qst, "user", "location")) {
    x$quoted_location <- qst$user$location
  } else {
    x$quoted_location <- NA_character_
  }
  x$is_quote <- !is.na(x$quoted_status_id)
  x[["quoted_status"]] <- NULL
  x
}

status_url_ <- function(x) {
  stopifnot(is.data.frame(x))
  if (all(c("screen_name", "status_id") %in% names(x))) {
    nas <- apply(
      x[, c("screen_name", "status_id")],
      1, function(x)
        all(is.na(x))
    )
    x$status_url <- paste0(
      "https://twitter.com/",
      x$screen_name,
      "/status/",
      x$status_id
    )
    x$status_url[nas] <- NA_character_
  } else {
    x$status_url <- NA_character_
  }
  x
}

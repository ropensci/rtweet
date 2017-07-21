
#' tweets_with_users
#'
#' @param x Data
#' @return Tbl
#' @export
tweets_with_users <- function(x) {
  tweets <- status_object(x)
  tweets <- tweets_df_(tweets)
  users <- user_object(x)
  users <- users_df_(users)
  attr(tweets, "users") <- users
  tweets
}

#' users_with_tweets
#'
#' @param x Data
#' @return Tbl
#' @export
users_with_tweets <- function(x) {
  tweets <- status_object(x)
  tweets <- tweets_df_(tweets)
  users <- user_object(x)
  users <- users_df_(users)
  if (nrow(tweets) == nrow(users)) {
    if (has_name(users, "user_id")) {
      tweets$user_id <- users$user_id
    if (has_name(users, "screen_name")) {
      tweets$screen_name <- users$screen_name
    }
    }
  }
  attr(users, "tweets") <- tweets
  users
}

##' tweets data
##'
##' Returns tweets data frame.
##' 
##' @title tweets_df
##' @param x Nested list parsed from Twitter's returned json object.
##' @return tbl
##' @export
tweets_df_ <- function(dat) {
  dat <- lapply(dat, tweets_to_tbl_)
  do.call("rbind", dat)
}

##' users data
##'
##' Returns users data frame.
##' 
##' @title users_df
##' @param x Nested list parsed from Twitter's returned json object.
##' @return tbl
##' @export
users_df_ <- function(x) {
  x <- lapply(x, users_to_tbl_)
  do.call("rbind", x)
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

`[**[` <- function(dat, x) {
  if (!is.recursive(dat) || length(dat) == 0L) {
    return(NA_character_)
  }
  if (!has_name(dat, x)) {
    dat[[x]] <- NA_character_
  }
  dat <- dat[[x]]
  dat[lengths(dat) == 0L] <- NA_character_
  dat
}

`[*[` <- function(dat, x) {
  if (all(
    is.list(dat),
    is.null(names(dat)),
    length(dat) > 0L
  )) {
    lapply(dat, "[[[", x)
  } else {
    `[[[`(dat, x)
  }
}

##  lapply(dat, function(i) {
##    i <- i[[x]]
##    if (length(i) == 0L) {
##      return(list(NA_character_))
##    }
##    i
##  })
##}

##-----------------------------------------------------
## vectorized safe extractors
##-----------------------------------------------------
gget <- function(x, ...) {
  dots <- list(...)
  for (i in dots) {
    x <- lapply(x, `[[[`, i)
  }
  x
}

##-----------------------------------------------------
## NSE version of gget
##-----------------------------------------------------
get_ <- function(x, ...) {
  dots <- as.character(eval(substitute(alist(...))))
  for (i in dots) {
    x <- lapply(x, `[*[`, i)
  }
  x
}


##-----------------------------------------------------
## make dim list
##-----------------------------------------------------
make_list_ <- function(x, what = NULL, n) {
  if (is.null(what)) what <- NA
  m_l_ <- function(x, n) {
    zips <- lengths(x) == 0L | is.na(x)
    x[zips] <- replicate(
      sum(zips),
      rep(what, length.out = n),
      simplify = FALSE
    )
    x
  }
  lapply(x, m_l_, n)
}

##-----------------------------------------------------
## get status object(s)
##-----------------------------------------------------
status_object <- function(x) {
  if (!(is.list(x) && is.null(names(x)))) {
    x <- list(x)
  }
  status_object_(x)
}
status_object_ <- function(x) {
  s_o_ <- function(x) {
    if (is.list(x) && length(x) == 1L) {
      x <- x[[1]]
    }
    if (is.list(x) && "statuses" %in% names(x)) {
      x <- x$statuses
    }
    if (`&&`(
      is.data.frame(x),
      c("text", "source") %in% names(x)
    )) {
      return(x)
    }
    if (`&&`(
      is.data.frame(x),
      "status" %in% names(x)
    )) {
      return(x$status)
    }
    data.frame()
  }
  lapply(x, s_o_)
}

has_name <- function(x, name) isTRUE(name %in% names(x))

tweets_to_tbl_ <- function(dat) {
  if (NROW(dat) == 0L) return(data.frame())
  ## extended entitites
  if (has_name(dat, "extended_entities") && has_name(dat[['extended_entities']], "media")) {
    dat$ext_media_url <- lapply(dat$extended_entities$media, "[[[", "media_url")
    dat$ext_media_expanded_url <- lapply(dat$extended_entities$media, "[[[", "expanded_url")
    dat$ext_media_t.co <- lapply(dat$extended_entities$media, "[[[", "url")
    dat$ext_media_type <- lapply(dat$extended_entities$media, "[[[", "type")
  } else {
    dat$ext_media_url <- as.list(NA_character_)
    dat$ext_media_expanded_url <- as.list(NA_character_)
    dat$ext_media_t.co <- as.list(NA_character_)
    dat$ext_media_type <- as.list(NA_character_)
  }
  if (has_name(dat, "entities") && has_name(dat[['entities']], "media")) {
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
  if (has_name(dat, "entities") && has_name(dat[["entities"]], "user_mentions")) {
    dat$mentions_user_id <- lapply(dat$entities$user_mentions, "[[[", "id_str")
    dat$mentions_screen_name <- lapply(dat$entities$user_mentions, "[[[", "screen_name")
  } else {
    dat$mentions_user_id <- as.list(NA_character_)
    dat$mentions_screen_name <- as.list(NA_character_)
  }
  if (has_name(dat, "entities") && has_name(dat[["entities"]], "hashtags")) {
    dat$hashtags <- lapply(dat$entities$hashtags, "[[[", "text")
  } else {
    dat$hashtags <- as.list(NA_character_)
  }
  if (has_name(dat, "entities") && has_name(dat[["entities"]], "symbols")) {
    dat$symbols <- lapply(dat$entities$symbols, "[[[", "text")
  } else {
    dat$symbols <- as.list(NA_character_)
  }
  if (has_name(dat, "geo") && has_name(dat[["geo"]], "coordinates")) {
    dat$geo_coords <- lapply(dat$geo$coordinates, `[[[`, 1, NA_ = c(NA_real_, NA_real_))
  } else {
    dat$geo_coords <- list(
        c(NA_real_, NA_real_)
      )
  }
  if (has_name(dat, "coordinates") && has_name(dat[["coordinates"]], "coordinates")) {
    dat$coordinates_coords <- lapply(dat$coordinates$coordinates, `[[[`, 1, NA_ = c(NA_real_, NA_real_))
  } else {
    dat$coordinates_coords <- list(
        c(NA_real_, NA_real_)
      )
  }
  if (has_name(dat, "place") && has_name(dat[["place"]], "id")) {
    dat$place_url <- `[[[`(dat$place, "url")
    dat$place_full_name <- `[[[`(dat$place, "full_name")
    dat$place_name <- `[[[`(dat$place, "name")
    dat$country_code <- `[[[`(dat$place, "country_code")
    dat$place_type <- `[[[`(dat$place, "place_type")
    dat$country <- `[[[`(dat$place, "country")
    if (has_name(dat$place, "bounding_box") && has_name(dat$place[["bounding_box"]], "coordinates")) {
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
        c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
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
        c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
      )
  }
  if (has_name(dat, "user") && has_name(dat[["user"]], "id_str")) {
    dat$user_id <- `[[[`(dat$user, "id_str")
    dat$screen_name <- `[[[`(dat$user, "screen_name")
  } else {
    dat$user_id <- NA_character_
    dat$screen_name <- NA_character_
  }
  ##dat <- wrangle_entities(dat)
  ##dat$bbox_coords <- bbox_coords_(dat)
  ##dat <- wrangle_place(dat)
  dat <- wrangle_quote_status(dat)
  dat <- wrangle_retweet_status(dat)
  ##dat$geo_coords <- geo_coords_(dat)
  ##dat$coords_coords <- coords_coords_(dat)
  statuscols <- statuscols_()
  nacols <- statuscols[!statuscols %in% names(dat)]
  for (i in nacols) {
    dat[[i]] <- NA
  }
  dat <- dat[, statuscols[statuscols %in% names(dat)]]
  names(dat) <- names(statuscols)[statuscols %in% names(dat)]
  
  dat$created_at <- format_date(dat$created_at)
  dat$source <- clean_source_(dat$source)
  tibble::as_tibble(dat, validate = FALSE)
}




##-----------------------------------------------------
## get user object(s)
##-----------------------------------------------------
user_object <- function(x) {
  if (!(is.list(x) && is.null(names(x)))) {
    x <- list(x)
  }
  user_object_(x)
}
user_object_ <- function(x) {
  u_o_ <- function(x) {
    if (is.list(x) && length(x) == 1L) {
      x <- x[[1]]
    }
    if (is.list(x) && "statuses" %in% names(x)) {
      x <- x$statuses
    }
    if (`&&`(
      is.data.frame(x),
      c("description", "location") %in% names(x)
    )) {
      return(x)
    }
    if (`&&`(
      is.data.frame(x),
      "user" %in% names(x)
    )) {
      return(x$user)
    }
    data.frame()
  }
  lapply(x, u_o_)
}

users_to_tbl_ <- function(dat) {
  if (nrow(dat) == 0L) return(data.frame())
  urls <- `[[[`(dat, "entities")
  urls <- `[[[`(urls, "url")
  urls <- `[[[`(urls, "urls")
  dat$profile_url <- unlist(
    lapply(urls, function(x) {
    if (is.data.frame(x)) x[["url"]] else NA_character_
    })
  )
  dat$profile_expanded_url <- unlist(
    lapply(urls, function(x) {
    if (is.data.frame(x)) x[["expanded_url"]] else NA_character_
    })
  )
  dat$created_at <- format_date(dat$created_at)
  usercols <- usercols_()
  nacols <- usercols[!usercols %in% names(dat)]
  for (i in nacols) {
    dat[[i]] <- NA
  }
  dat <- dat[, usercols[usercols %in% names(dat)]]
  names(dat) <- names(usercols)[usercols %in% names(dat)]
  tibble::as_tibble(dat, validate = FALSE)
}





##-----------------------------------------------------
## column names
##-----------------------------------------------------
usercols_ <- function() {
  c(
  user_id = "id_str",
  name = "name",
  screen_name = "screen_name",
  location = "location",
  description = "description",
  url = "url",
  protected = "protected",
  followers_count = "followers_count",
  friends_count = "friends_count",
  listed_count = "listed_count",
  statuses_count = "statuses_count",
  favourites_count = "favourites_count",
  account_created_at = "created_at",
  verified = "verified",
  profile_url = "profile_url",
  profile_expanded_url = "profile_expanded_url",
  account_lang = "lang",
  profile_banner_url = "profile_banner_url",
  profile_background_url = "profile_background_image_url",
  profile_image_url = "profile_image_url"
)
}

statuscols_ <- function() {
  c(
  status_id = "id_str",
  created_at = "created_at",
  user_id = "user_id",
  screen_name = "screen_name",
  text = "text",
  source = "source",
  reply_to_status_id = "in_reply_to_status_id_str",
  reply_to_user_id = "in_reply_to_user_id_str",
  reply_to_screen_name = "in_reply_to_screen_name",
  is_quote = "is_quote",
  is_retweet = "is_retweet",
  favorite_count = "favorite_count",
  retweet_count = "retweet_count",
  hashtags = "hashtags",
  symbols = "symbols",
  media_url = "media_url",
  media_t.co = "media_t.co",
  media_expanded_url = "media_expanded_url",
  media_type = "media_type",
  ext_media_url = "ext_media_url",
  ext_media_t.co = "ext_media_t.co",
  ext_media_expanded_url = "ext_media_expanded_url",
  ext_media_type = "ext_media_expanded_type",
  mentions_user_id = "mentions_user_id",
  mentions_screen_name = "mentions_screen_name",
  lang = "lang",
  quoted_status_id = "quoted_status_id",
  quoted_text = "quoted_text",
  retweet_status_id = "retweet_status_id",
  retweet_text = "retweet_text",
  place_url = "place_url",
  place_type = "place_type",
  place_name = "place_name",
  country = "country",
  country_code = "country_code",
  geo_coords = "geo_coords",
  coords_coords = "coordinates_coords",
  bbox_coords = "bbox_coords"
)
}

##-----------------------------------------------------
## utility funs
##-----------------------------------------------------
m_l_ <- function(x, what, n) {
    zips <- lengths(x) == 0L | is.na(x)
    x[zips] <- replicate(
      sum(zips),
      rep(what, length.out = n),
      simplify = FALSE)
    x
}

clean_source_ <- function(x) {
  unlist(lapply(
    strsplit(as.character(x), "[<]|[>]"),
    function(x) x[3]), use.names = FALSE)
}

##-----------------------------------------------------
## coordinate extraction funs
##-----------------------------------------------------
geo_coords_ <- function(dat) {
  if (!has_name(dat, "geo")) return(list(NA_real_, NA_real_))
  dat <- `[[[`(dat, "geo")
  if (!has_name(dat, "coordinates")) return(list(NA_real_, NA_real_))
  dat <- `[[[`(dat, "coordinates")
  m_l_(dat, NA_real_, 2)
}

coords_coords_ <- function(dat) {
  if (!has_name(dat, "coordinates")) return(list(NA_real_, NA_real_))
  dat <- `[[[`(dat, "coordinates")
  if (!has_name(dat, "coordinates")) return(list(NA_real_, NA_real_))
  dat <- `[[[`(dat, "coordinates")
  m_l_(dat, NA_real_, 2)
}

bbox_coords_ <- function(dat) {
  if (!has_name(dat, "place")) return(list(NA_real_, NA_real_))
  dat <- `[[[`(dat, "place")
  if (!has_name(dat, "bounding_box")) return(list(NA_real_, NA_real_))
  dat <- `[[[`(dat, "bounding_box")
  if (!has_name(dat, "coordinates")) return(list(NA_real_, NA_real_))
  dat <- `[[[`(dat, "coordinates")
  dat <- lapply(dat, function(x) {
    if (is.array(x)) c(x[, , 1], x[, , 2]) else rep(NA_real_, 8)
  })
  dat
  ##m_l_(dat, NA_real_, 8)
}


##-----------------------------------------------------
## extracting nested objects
##-----------------------------------------------------
#' @importFrom utils hasName
wrangle_entities <- function(x) {
  n <- nrow(x)
  if (has_name(x, "entities")) {
    ent <- x[["entities"]]
  } else {
    ent <- data.frame()
  }
  ## user mentions
  if (has_name(ent, "user_mentions")) {
    x$mentions_user_id <- lapply(ent$user_mentions, "[[[", "id_str")
    x$mentions_screen_name <- lapply(ent$user_mentions, "[[[", "screen_name")
  } else {
    x$mentions_user_id <- NA_character_
    x$mentions_screen_name <- NA_character_
  }
  ## media object
  if (hasName(ent, "media")) {
    x$media_t.co <- lapply(ent$media, "[[[", "url")
    x$media_url <- lapply(ent$media, "[[[", "media_url")
    x$media_expanded_url <- lapply(ent$media, "[[[", "expanded_url")
  } else {
    x$media_t.co_url <- NA_character_
    x$media_url <- NA_character_
    x$media_expanded_url <- NA_character_
  }
  ## hashtags
  if (hasName(ent, "hashtags")) {
    x$hashtags <- lapply(ent$hashtags, "[[[", "text")
  } else {
    x$hashtags <- NA_character_
  }
  ## symbols
  if (hasName(ent, "symbols")) {
    x$symbols <- lapply(ent$symbols, "[[[", "text")
  } else {
    x$symbols <- NA_character_
  }
  x$entities <- NULL
  x
}

#' @importFrom utils hasName
wrangle_retweet_status <- function(x) {
  n <- nrow(x)
  if (has_name(x, "retweeted_status")) {
    rst <- x[["retweeted_status"]]
  } else {
    rst <- data.frame()
  }
  ## user mentions
  if (has_name(rst, "id_str")) {
    x$retweet_status_id <- rst$id_str
  } else {
    x$retweet_status_id <- NA_character_
  }
  if (has_name(rst, "full_text")) {
    x$retweet_text <- rst$full_text
  } else {
    x$retweet_text <- NA_character_
  }
  x$is_retweet <- !is.na(x$retweet_status_id)
  x[["retweeted_status"]] <- NULL
  x
}

#' @importFrom utils hasName
wrangle_quote_status <- function(x) {
  n <- nrow(x)
  if (has_name(x, "quoted_status")) {
    qst <- x[["quoted_status"]]
  } else {
    qst <- data.frame()
  }
  ## user mentions
  if (has_name(qst, "id_str")) {
    x$quoted_status_id <- qst$id_str
  } else {
    x$quoted_status_id <- NA_character_
  }
  if (has_name(qst, "full_text")) {
    x$quoted_text <- qst$full_text
  } else {
    x$quoted_text <- NA_character_
  }
  x$is_quote <- !is.na(x$quoted_status_id)
  x[["quoted_status"]] <- NULL
  x
}


#' @importFrom utils hasName
wrangle_place <- function(x) {
  n <- nrow(x)
  if (has_name(x, "place")) {
    plc <- x[["place"]]
  } else {
    plc <- data.frame()
  }
  ## user mentions
  if (has_name(plc, "full_name")) {
    x$place_name <- plc$full_name
  } else {
    x$place_name <- NA_character_
  }
  if (has_name(plc, "place_type")) {
    x$place_type <- plc$place_type
  } else {
    x$place_type <- NA_character_
  }
  if (has_name(plc, "country")) {
    x$country <- plc$country
  } else {
    x$country <- NA_character_
  }
  if (has_name(plc, "place_url")) {
    x$place_url <- plc$place_url
  } else {
    x$place_url <- NA_character_
  }
  x$place <- NULL
  x
}


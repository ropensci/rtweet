
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
tweets_df_ <- function(x) {
  x <- lapply(x, tweets_to_tbl_)
  do.call("rbind", x)
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
## safe extractor
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
      simplify = FALSE)
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

tweets_to_tbl_ <- function(dat) {
  dat <- wrangle_entities(dat)
  dat$bbox_coords <- bbox_coords_(dat)
  dat <- wrangle_place(dat)
  dat <- wrangle_quote_status(dat)
  dat <- wrangle_retweet_status(dat)
  dat$geo_coords <- geo_coords_(dat)
  dat$coords_coords <- coords_coords_(dat)
  statuscols <- statuscols_()
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
  profile_banner = "profile_banner_url",
  profile_bg = "profile_background_image_url",
  profile_image = "profile_image_url"
)
}

statuscols_ <- function() {
  c(
  status_id = "id_str",
  created_at = "created_at",
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
  media_expanded_url = "media_expanded_url",
  mentions_user_id = "mentions_user_id",
  mentions_screen_name = "mentions_screen_name",
  lang = "lang",
  quoted_status_id = "quoted_status_id",
  retweet_status_id = "retweet_status_id",
  place_url = "place_url",
  place_type = "place_type",
  place_name = "place_name",
  country = "country",
  geo_coords = "geo_coords",
  coords_coords = "coords_coords",
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
  dat <- `[[[`(dat, "geo")
  dat <- `[[[`(dat, "coordinates")
  m_l_(dat, NA_real_, 2)
}

coords_coords_ <- function(dat) {
  dat <- `[[[`(dat, "coordinates")
  dat <- `[[[`(dat, "coordinates")
  m_l_(dat, NA_real_, 2)
}

bbox_coords_ <- function(dat) {
  dat <- `[[[`(dat, "place")
  dat <- `[[[`(dat, "bounding_box")
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
  if (hasName(x, "entities")) {
    ent <- x[["entities"]]
  } else {
    ent <- data.frame()
  }
  ## user mentions
  if (hasName(ent, "user_mentions")) {
    x$mentions_user_id <- lapply(ent$user_mentions, "[[[", "id_str")
    x$mentions_screen_name <- lapply(ent$user_mentions, "[[[", "screen_name")
  } else {
    x$mentions_user_id <- NA_character_
    x$mentions_screen_name <- NA_character_
  }
  ## media object
  if (hasName(ent, "media")) {
    if (hasName(ent, "url") && hasName(ent, "expanded_url")) {
      x$media_url <- ent$media$url
      x$media_expanded_url <- ent$media_expanded_url
    } else {
      x$media_url <- NA_character_
      x$media_expanded_url <- NA_character_
    }
  } else {
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
  if (hasName(x, "retweeted_status")) {
    rst <- x[["retweeted_status"]]
  } else {
    rst <- data.frame()
  }
  ## user mentions
  if (hasName(rst, "id_str")) {
    x$retweet_status_id <- rst$id_str
  } else {
    x$retweet_status_id <- NA_character_
  }
  x$is_retweet <- !is.na(x$retweet_status_id)
  x[["retweeted_status"]] <- NULL
  x
}

#' @importFrom utils hasName
wrangle_quote_status <- function(x) {
  n <- nrow(x)
  if (hasName(x, "quoted_status")) {
    qst <- x[["quoted_status"]]
  } else {
    qst <- data.frame()
  }
  ## user mentions
  if (hasName(qst, "id_str")) {
    x$quoted_status_id <- qst$id_str
  } else {
    x$quoted_status_id <- NA_character_
  }
  x$is_quote <- !is.na(x$quoted_status_id)
  x[["quoted_status"]] <- NULL
  x
}


#' @importFrom utils hasName
wrangle_place <- function(x) {
  n <- nrow(x)
  if (hasName(x, "place")) {
    plc <- x[["place"]]
  } else {
    plc <- data.frame()
  }
  ## user mentions
  if (hasName(plc, "full_name")) {
    x$place_name <- plc$full_name
  } else {
    x$place_name <- NA_character_
  }
  if (hasName(plc, "place_type")) {
    x$place_type <- plc$place_type
  } else {
    x$place_type <- NA_character_
  }
  if (hasName(plc, "country")) {
    x$country <- plc$country
  } else {
    x$country <- NA_character_
  }
  if (hasName(plc, "place_url")) {
    x$place_url <- plc$place_url
  } else {
    x$place_url <- NA_character_
  }
  x$place <- NULL
  x
}


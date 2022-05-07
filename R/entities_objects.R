# Contains functions to parse the objects described here: 
# https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/object-model/entities

# <https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/object-model/entities#hashtags>
hashtags <- function(x) {
  if (NROW(x) == 0) {
    data.frame(text = NA, indices = I(list(NA)), 
                      stringsAsFactors = FALSE)
  } else {
    i <- indices_vec(x$indices)
    data.frame(text = x$text, indices = I(i))
  }
}

# PowerTrack $ text
# has:symbol
# They are the same
# <https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/object-model/entities#symbols>
# parse_entities2 uses the name of the columns to match the appropriate function to parse it. 
# It needs a symbols function that is the same as hashtags
symbols <- hashtags

indices_vec <- function(x) {
  lapply(x, function(y){
    matrix(y, ncol = 2, dimnames = list(NULL, c("start", "end")))})
}

# The extended entities is really for media
# <https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/object-model/extended-entities>
media <- function(x) {
  if (NROW(x) == 0) {
    df <- data.frame(id = NA, id_str = NA, indices = I(list(NA)), 
                     media_url = NA, media_url_https = NA, 
                     url = NA, display_url = NA, expanded_url = NA, 
                     type = NA, sizes = I(list(NA)), ext_alt_text = NA,
                     stringsAsFactors = FALSE)
    return(df)
  }
  indices <- as.data.frame(t(simplify2array(x$indices)))
  colnames(indices) <- c("start", "end")
  x$indices <- I(list(indices))
  sizes <- rbind(x$sizes$large, x$sizes$small, x$sizes$thumb, x$sizes$medium)
  sizes$type <- c("large", "small", "thumb", "medium")
  x$sizes <- list(sizes)
  df_colnames <- c("id", "id_str", "indices", "media_url", "media_url_https", 
                   "url", "display_url", "expanded_url", "type", "sizes", 
                   "ext_alt_text")
  x[setdiff(df_colnames, colnames(x))] <- rep(NA, nrow(x))
  x
}

urls <- function(x) {
  if (NROW(x) == 0) {
    df <- data.frame(url = NA, expanded_url = NA, display_url = NA, 
                     indices = I(list(NA)), unwound = I(list(NA)), 
                     stringsAsFactors = FALSE)
    return(df)
  }
  indices <- as.data.frame(t(simplify2array(x$indices)))
  colnames(indices) <- c("start", "end")
  x$indices <- I(indices)
  x[setdiff(c("url", "expanded_url", "display_url", "indices", "unwound"), 
            colnames(x))] <- rep(NA, nrow(x))
  x
}

# PowerTrack @ screen_name
# has:mentions
# <https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/object-model/entities#mentions>
user_mentions <- function(x) {
  if (NROW(x) == 0) {
    df <- data.frame(screen_name = NA, name = NA, id = NA, id_str = NA,
                     indices = I(list(NA)), stringsAsFactors = FALSE)
    return(df)
  }
  indices <- as.data.frame(t(simplify2array(x$indices)))
  colnames(indices) <- c("start", "end")
  x$indices <- indices
  df_colnames <- c("screen_name", "name", "id", "id_str", "indices")
  x[setdiff(df_colnames, colnames(x))] <- rep(NA, nrow(x))
  rownames(x) <- NULL
  x
}

# <https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/object-model/entities#polls>
# Not testable without fullarchive access
polls <- function(x) {
  df <- data.frame(options= I(list(NA)), end_datetime = NA, 
                   duration_minutes = NA, stringsAsFactors = FALSE)
  if (NROW(x) == 0) {
    return(df)
  } 
  x[setdiff(colnames(df), colnames(x))] <- rep(NA, nrow(x))
  x
}


parse_entities <- function(x) {
  
  if (is.null(x)) {
    return(list(description = urls(NULL), url = urls(NULL)))
  }
  
  if (is.null(x$description$urls)) {
    description <- list(description = urls(x$description$urls))
  } else {
    description <- lapply(x$description$urls, urls)
    
  }
  
  if (is.null(x$url$urls)) {
    url <- list(url = urls(x$url$urls))
  } else {
    url <- lapply(x$url$urls, urls)
  }
  l <- Map(list, description, url)
  lapply(l, `names<-`, value = c("description", "url"))
}


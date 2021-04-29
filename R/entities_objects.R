# Contains functions to parse the objects described here: 
# https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/object-model/entities

# <https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/object-model/entities#hashtags>
hashtags <- function(x) {
  if (nrow(x) == 0) {
    return(data.frame(text = NA, indices = I(list(NA))))
  }
  tibble::tibble(text = x$text, indices = list(simplify2array(x$indices)))
}

# The extended entities is really for media
# <https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/object-model/extended-entities>
media <- function(x) {
  if (nrow(x) == 0) {
    df <- data.frame("id" = NA, "id_str" = NA, "indices" = I(list(NA)), 
                     "media_url" = NA, "media_url_https" = NA, 
                     "url" = NA, "display_url" = NA, "expanded_url" = NA, 
                     "type" = NA, "sizes" = I(list(NA)))
    return(df)
  }
  indices <- as.data.frame(t(simplify2array(x$indices)))
  colnames(indices) <- c("start", "end")
  x$indices <- list(indices)
  sizes <- rbind(x$sizes$large, x$sizes$small, x$sizes$thumb, x$sizes$medium)
  sizes$type <- c("large", "small", "thumb", "medium")
  x$sizes <- list(sizes)
  x
}

urls <- function(x) {
  if (nrow(x) == 0) {
    df <- data.frame("url" = NA, "expanded_url" = NA, "display_url" = NA, 
                     "indices" = I(list(NA)), "unwound" = I(list(NA)))
    return(df)
  }
  indices <- as.data.frame(t(simplify2array(x$indices)))
  colnames(indices) <- c("start", "end")
  x$indices <- indices
  x
}

# PowerTrack @ screen_name
# has:mentions
# <https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/object-model/entities#mentions>
user_mentions <- function(x) {
  if (nrow(x) == 0) {
    df <- data.frame("screen_name" = NA, "name" = NA, "id" = NA, "id_str" = NA,
                     "indices" = I(list(NA)))
    return(df)
  }
  indices <- as.data.frame(t(simplify2array(x$indices)))
  colnames(indices) <- c("start", "end")
  x$indices <- indices
  x
}

# PowerTrack $ text
# has:symbol
# They are the same
# <https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/object-model/entities#symbols>
symbols <- hashtags

# <https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/object-model/entities#polls>
# Not testable without fullarchive access
polls <- function(x) {
    if (is.null(x)) {
      return(data.frame("options"= I(list(NA)), "end_datetime" = NA, "duration_minutes" = NA))
    } else {
      x
    }
}



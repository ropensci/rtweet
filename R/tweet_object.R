tweet <- function(x) {
  empty <- data.frame("created_at" = NA_character_, "id" = NA_integer_, 
                      "id_str" = NA_character_, "text" = NA_character_, 
                      "truncated" = NA, "entities" = I(list(list())), 
                      "source" = NA_character_, 
                      "in_reply_to_status_id" = NA_integer_, 
                      "in_reply_to_status_id_str" = NA_character_, 
                      "in_reply_to_user_id" = NA_integer_,
                      "in_reply_to_user_id_str" = NA_character_,
                      "in_reply_to_screen_name" = NA_character_, 
                      "geo" = NA, "coordinates" = NA, "place" = NA, 
                      "contributors" = NA, "is_quote_status" = NA, 
                      "retweet_count" = 0, "favorite_count" = 0, 
                      "favorited" = NA, "retweeted" = NA, 
                      "lang" = NA_character_,
                      "possibly_sensitive" = NA,
                      "display_text_width" = NA)
  if (NROW(x) == 0) {
    return(as_tbl(empty))
  }
  
  tb <- as_tbl(x[c("created_at", "id", "id_str", "truncated", "source", 
             "in_reply_to_status_id", "in_reply_to_status_id_str", "in_reply_to_user_id", 
             "in_reply_to_user_id_str", "in_reply_to_screen_name", "geo", 
             "coordinates", "place", "contributors", "is_quote_status", "retweet_count", 
             "favorite_count", "favorited", "retweeted", 
             "lang")])
  
  
  #  Some fields seem to depend on what is needed
  # possibly_sensitive, full_text, extended_entities
  if (has_name_(x, "possibly_sensitive")) {
    tb$possibly_sensitive <- x$possibly_sensitive
  } else {
    tb$possibly_sensitive <- FALSE
  }
  
  if (has_name_(x, "text")) {
    tb$text <- x$text
    tb$display_text_width <- nchar(x$text)
  } else if (has_name_(x, "full_text")) {
    tb$text <- x$full_text
    tb$display_text_width <- vapply(x$display_text_range, `[`, numeric(1), i = 2)
  }
  user <- user(x$user)
  l <- split(user, seq_len(NROW(user)))
  names(l) <- NULL
  tb$user <- l
  
  if (has_name_(x, "entities") && has_name_(x, "extended_entities")) {
    ent <- parse_entities2(x$entities)
    ext_ent <- parse_entities2(x$extended_entities)
    for (i in NROW(x$entities)) {
      ent[[i]][names(x$extended_entities)] <- ext_ent[[i]][names(x$extended_entities)]
    }
    
  } else if (has_name_(x, "entities")) {
    ent <- parse_entities2(x$entities)
  } else if (has_name_(x, "extended_entities")) {
    ent <- parse_entities2(x$extended_entities)
  }  else {
    ent <- vector("list", NROW(x$entities))
  }
  tb$entities <- ent
  
  as_tbl(tb)
}

parse_entities2 <- function(y) {
  l <- vector("list", NROW(y))
  for (col in seq_len(NCOL(y))) {
    # Look for the function of said object and save it. 
    fun <- match.fun(colnames(y)[col])
    l[[col]] <- lapply(y[[col]], fun)
  }
  # Split and join
  ll <- transpose_list(l)
  lapply(ll, `names<-`, value = colnames(y))
}

# From https://stackoverflow.com/a/54970694/2886003
# Assumes equal length for each list on the list
transpose_list <- function(l) {
  l2 <- split(do.call(cbind, l), seq_len(length(l[[1]])))
  names(l2) <- NULL
  l2
}

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
  # Select columns that do not need tweaks and add those that are missing
  orig_names <- names(x)
  v_names <- c("created_at", "id", "id_str", "truncated", "source", 
               "in_reply_to_status_id", "in_reply_to_status_id_str", "in_reply_to_user_id", 
               "in_reply_to_user_id_str", "in_reply_to_screen_name", "geo", 
               "coordinates", "place", "contributors", "is_quote_status", 
               "quoted_status_id", "quoted_status_id_str", "quoted_status_permalink",
               "retweet_count", 
               "favorite_count", "favorited", "retweeted", 
               "lang")
  
  tb <- as_tbl(x[v_names %in% orig_names])
  tb[v_names[!v_names %in% colnames(tb)]] <- NA
  
  
  #  Some fields seem to depend on what is needed
  # possibly_sensitive, full_text, extended_entities
  if (has_name_(x, "possibly_sensitive")) {
    tb$possibly_sensitive <- x$possibly_sensitive
  } else {
    tb$possibly_sensitive <- FALSE
  }
  
  if (has_name_(x, "quoted_status")) {
    tb$quoted_status <- tweet(x$quoted_status) # Recursive and in a loop not good..
  } else {
    tb$quoted_status <- FALSE
  }
  
  if (has_name_(x, "display_text_range")) {
    tb$display_text_range <- vapply(x$display_text_range, `[`, numeric(1), i = 2)
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
  
  tb$coordinates <- coordinates(x$coordinates)
  tb$place <- place(x$place)
  
  tb$entities <- ent
  stopifnot(all(orig_names  %in% colnames(tb)))
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


quoted_status <- function(x) {
  c("created_at", "id", "id_str", "full_text", "truncated", "display_text_range", 
    "entities", "source", "in_reply_to_status_id", "in_reply_to_status_id_str", 
    "in_reply_to_user_id", "in_reply_to_user_id_str", "in_reply_to_screen_name", 
    "user", "geo", "coordinates", "place", "contributors", "is_quote_status", 
    "retweet_count", "favorite_count", "favorited", "retweeted", 
    "possibly_sensitive", "lang")
}

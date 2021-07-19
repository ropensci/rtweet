tweet <- function(x) {
  empty <- data.frame(created_at = NA_character_, id = NA_integer_, 
                      id_str = NA_character_, 
                      text = NA_character_, 
                      full_text = NA_character_,
                      truncated = NA, 
                      entities = I(list(list())), 
                      source = NA_character_, 
                      in_reply_to_status_id = NA_integer_, 
                      in_reply_to_status_id_str = NA_character_, 
                      in_reply_to_user_id = NA_integer_,
                      in_reply_to_user_id_str = NA_character_,
                      in_reply_to_screen_name = NA_character_, 
                      geo = NA, 
                      coordinates = NA, place = NA, 
                      contributors = NA, is_quote_status = NA, 
                      retweet_count = 0, favorite_count = 0, 
                      favorited = NA, favorited_by = NA,
                      retweeted = NA, 
                      lang = NA_character_,
                      possibly_sensitive = NA,
                      display_text_width = NA,
                      display_text_range = NA,
                      retweeted_status = NA,
                      quoted_status = NA,
                      quoted_status_id = NA,
                      quoted_status_id_str = NA, 
                      quoted_status_permalink = NA,
                      metadata = NA,
                      query = NA,
                      user = I(list(list())),
                      possibly_sensitive_appealable = NA)
  if (NROW(x) == 0) {
    return(empty)
  }
  
  tb <- x
  #  Some fields seem to depend on what is needed
  # possibly_sensitive, full_text, extended_entities
  if (has_name_(x, "possibly_sensitive")) {
    tb$possibly_sensitive <- x$possibly_sensitive
  } else {
    tb$possibly_sensitive <- list(NA)
  }
  
  # Recursive and in a loop not good..
  if (has_name_(x, "quoted_status")) {
    tb$quoted_status <- tweet(x$quoted_status) 
  } else {
    tb$quoted_status <- list(NA)
  }
  
  if (has_name_(x, "display_text_range")) {
    # Handle missing display_text_range
    tb$display_text_range <- display_text_range(x$display_text_range)
  }
  
  if (has_name_(x, "quoted_status_permalink")){
    tb$quoted_status_permalink <- split_df(x$quoted_status_permalink)
  }
  if (has_name_(x, "retweeted_status")){
    tb$retweeted_status <- split_df(x$retweeted_status)
  }
  if (has_name_(x, "quoted_status")){
    tb$quoted_status <- split_df(x$quoted_status)
  }
  if (has_name_(x, "metadata")){
    tb$metadata <- split_df(x$metadata)
  }
  
  if (has_name_(x, "text")) {
    tb$text <- x$text
    tb$display_text_width <- nchar(x$text)
  } else if (has_name_(x, "full_text")) {
    tb$text <- x$full_text
  }
  user <- user(x[["user"]])
  tb$user <- split_df(user)
  
  if (has_name_(x, "entities") && has_name_(x, "extended_entities")) {
    ent <- parse_entities2(x$entities)
    ext_ent <- parse_entities2(x$extended_entities)
    tb$extended_entities <- NULL
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
  
  tb$coordinates <- lapply(x$coordinates, coordinates)
  if (is.data.frame(x$place)) {
    l <- split_df(x$place)
    tb$place <- lapply(l, place)
  } else {
    tb$place <- lapply(x$place, place)
  }
  
  end <- setdiff(colnames(tb), colnames(empty))
  if (length(end) != 0) {
    stop(end)
  }
  tb[setdiff(colnames(empty), colnames(tb))] <- NA
  tb
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


split_df <- function(x) {
  l <- split(x, seq_len(NROW(x)))
  names(l) <- NULL
  l
}

display_text_range <- function(x) {
  ldtr <- lengths(x)
  dtr <- rep(NA, length.out = length(x))
  dtr[ldtr != 0] <- vapply(x[ldtr != 0], `[`, numeric(1), i = 2)
  dtr
}

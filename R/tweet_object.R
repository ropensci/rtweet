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
                      geo = I(list(list())),
                      coordinates = NA, place = NA,
                      contributors = NA, is_quote_status = NA,
                      retweet_count = 0, favorite_count = 0,
                      favorited = NA, favorited_by = NA,
                      retweeted = NA,
                      scopes = I(list(list())),
                      lang = NA_character_,
                      possibly_sensitive = NA,
                      display_text_width = NA,
                      display_text_range = NA,
                      retweeted_status = NA,
                      quoted_status = NA,
                      quoted_status_id = NA,
                      quoted_status_id_str = NA,
                      quoted_status_permalink = NA,
                      quote_count = NA,
                      timestamp_ms = NA,
                      reply_count = NA,
                      filter_level = NA,
                      metadata = NA,
                      query = NA,
                      user = I(list(list())),
                      withheld_scope = NA_character_,
                      withheld_copyright = NA,
                      withheld_in_countries = NA_character_,
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
  if (has_name_(x, "scopes")){
    tb$scopes <- split_df(x$scopes)
  }
  if (has_name_(x, "geo")){
    tb$geo <- split_df(x$geo)
  }

  # If extended_tweet is present it also have the full_text field and extended_entities
  # Add the columns and use the columns if available
  # columns: "full_text", "display_text_range", "entities", "extended_entities"
  if (has_name_(x, "extended_tweet")) {
    x <- cbind(x, x[, "extended_tweet"])
    x[, "extended_tweet"] <- NULL
    tb$extended_tweet <- NULL
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

  tb$coordinates <- split_df(coordinates(x$coordinates))
  if (is.data.frame(x$place)) {
    l <- split_df(x$place)
    tb$place <- lapply(l, place)
  } else {
    tb$place <- lapply(x$place, place)
  }


  end <- setdiff(names(tb), colnames(empty))
  # Omit extended tweet info from stream API v1 and premium API: should be handled now
  # Omit matching_rules just from premium API v1 (not sure how it works)
  # Omit edit fields as they will be handled in API v2
  edits <- c("edit_history", "edit_controls", "editable")
  end <- setdiff(end, c(edits, "matching_rules"))
  if (is.data.frame(x) && length(end) != 0) {
    warning("Unidentified value: ", paste(end, collapse = ", "),
         ".\n\tPlease open an issue to notify the maintainer. Thanks!", call. = FALSE)
  }
  tb[setdiff(names(empty), names(tb))] <- NA
  # Skip this new fields I will
  tb[, edits] <- NULL
  rownames(tb) <- NULL
  if (!is.data.frame(tb)) {
    k <- lengths(tb) == 0
    tb[names(k)[k]] <- empty[names(k)[k]]
    tb <- list2DF(tb[names(empty)])
  }
  tb
}

parse_entities2 <- function(y) {
  if (!is.data.frame(y) && is.list(y)) {
    l <- vector("list", length(y))
  } else {
    l <- vector("list", NROW(y))
  }
  for (col in seq_along(y)) {
    # Look for the function of said object and save it.
    if (names(y)[col] != "user_mentions")  {
      fun <- match.fun(names(y)[col])
    } else {
      fun <- match.fun("user_mention")
    }
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
  if (is.null(nrow(x))) {
    return(list(list()))
  }
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

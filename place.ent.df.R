
x <- qresp$statuses

#ent
entitites_df <- dplyr::data_frame(user_mentions = x$entities[["user_mentions"]] %>% map("id_str"),
hashtags = x$entities[["hashtags"]] %>% map("text"),
urls = x$entities[["urls"]] %>% map("expanded_url"))

entitites_df[, ] <- apply(entitites_df, 2, function(x) ifelse(sapply(x, is.null), NA, unlist(x)))

# place
place_df <- dplyr::data_frame(
  place_id = tryCatch(x$place[["id"]], error = function(e) return(rep(NA, nrow(x)))),
  place_url = tryCatch(x$place[["url"]], error = function(e) return(rep(NA, nrow(x)))),
  place_full_name = tryCatch(x$place[["full_name"]], error = function(e) return(rep(NA, nrow(x)))),
  place_country = tryCatch(x$place[["country"]], error = function(e) return(rep(NA, nrow(x)))),
  place_coordinates = tryCatch(lapply(
    x$place$bounding_box$coordinates, function(z)
      if(!is.null(z)) flatten_dbl(as.data.frame(z))),
    error = function(e) return(rep(NA, nrow(x)))))



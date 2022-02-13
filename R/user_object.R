# Documents <https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/object-model/user>


user <- function(x) {
 empty <- data.frame(
   "id" = NA_integer_, "id_str" = NA_character_,
   "name" = NA_character_, "screen_name" = NA_character_,
   "location" = NA_character_, "derived" = NA_character_,
   "url" = NA_character_, "description" = NA_character_,
   "protected" = NA, "verified" = NA, "followers_count" = NA_integer_, 
   "friends_count" = NA_integer_, "listed_count" = NA_character_,
   "favourites_count" = NA_integer_, "statuses_count" = NA_integer_,
   "created_at" = NA_character_, "profile_banner_url" = NA_character_, 
   "profile_image_url_https" = NA_character_,
   "default_profile" = NA, "default_profile_image" = NA,
   "withheld_in_countries" = I(list(list())),
   "withheld_scope" = NA, stringsAsFactors = FALSE
 )
 empty <- empty
 if (NROW(x) == 0) {
    return(empty)
 } 
 
 # Ignoring status, as it holds tweets
 y <- x[ , colnames(x) %in% colnames(empty)]
 
 # Adding missing values.
 missing <- setdiff(colnames(empty), colnames(y))
 if (length(missing) != 0 ) {
    y[ , missing] <- empty[rep(1, nrow(y)), missing]
 }
 
 if (has_name_(x, "entities")) {
    y$entities <- parse_entities(x$entities)
 } else {
    y$entitites <- list(list())
 }
 y
}

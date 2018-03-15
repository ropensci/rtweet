

users_names <- function() {
  c("user_id", "screen_name", "name", "location", "description", "url", "protected", "followers_count", "friends_count", "listed_count", "statuses_count", "favourites_count", "account_created_at", "verified", "profile_url", "profile_expanded_url", "account_lang", "profile_banner_url", "profile_background_url", "profile_image_url")
}

names_in_users <- function(x) {
  usrnms <- users_names()
  usrnms <- usrnms[usrnms != c("user_id", "screen_name")]
  sum(names(x) %in% usrnms)
}

names_in_tweets <- function(x) {
  twnms <- tweets_names()
  twnms <- twnms[twnms != c("user_id", "screen_name")]
  sum(names(x) %in% twnms)
}

tweets_names <- function() {
  c("user_id", "status_id", "created_at", "screen_name", "text", "source", "display_text_width", "reply_to_status_id", "reply_to_user_id", "reply_to_screen_name", "is_quote", "is_retweet", "favorite_count", "retweet_count", "hashtags", "symbols", "urls_url", "urls_t.co", "urls_expanded_url", "media_url", "media_t.co", "media_expanded_url", "media_type", "ext_media_url", "ext_media_t.co", "ext_media_expanded_url", "ext_media_type", "mentions_user_id", "mentions_screen_name", "lang", "quoted_status_id", "quoted_text", "quoted_created_at", "quoted_source", "quoted_favorite_count", "quoted_retweet_count", "quoted_user_id", "quoted_screen_name", "quoted_name", "quoted_followers_count", "quoted_friends_count", "quoted_statuses_count", "quoted_location", "quoted_description", "quoted_verified", "retweet_status_id", "retweet_text", "retweet_created_at", "retweet_source", "retweet_favorite_count", "retweet_user_id", "retweet_screen_name", "retweet_name", "retweet_followers_count", "retweet_friends_count", "retweet_statuses_count", "retweet_location", "retweet_description", "retweet_verified", "place_url", "place_name", "place_full_name", "place_type", "country", "country_code", "geo_coords", "coords_coords", "bbox_coords")
}


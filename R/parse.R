#' parse_status
#'
#' @description Converts Twitter status object to neat data_frame.
#' @param x json object
#' @import dplyr
#' @export
parse_status <- function(x) {

  if (sum(x$is_quote_status, na.rm = TRUE) == 0) {
    x$quoted_status_id_str <- NA
  }

  df <- data_frame(
    status_id = x$id_str,
    text = x$text,
    in_reply_to_status_id = x$in_reply_to_status_id_str,
    in_reply_to_user_id  = x$in_reply_to_user_id_str,
    in_reply_to_screen_name = x$in_reply_to_screen_name,
    is_quote_status = x$is_quote_status,
    retweet_count = x$retweet_count,
    favorite_count = x$favorite_count,
    lang = x$lang,
    quoted_status_id = x$quoted_status_id_str,
    hashtags = I(
      lapply(x$entities$hashtags,
        function(x) getElement(x, "text"))),
    user_mentions_screen_name = I(
      lapply(x$entities$user_mentions,
        function(x) getElement(x, "screen_name"))),
    user_mentions_user_id = I(
      lapply(x$entities$user_mentions,
        function(x) getElement(x, "id_str"))),
    urls_expanded_url = I(
      lapply(x$entities$urls,
        function(x) getElement(x, "expanded_url"))))

  df
}

#' .parse_retweet
#'
#' @param x json response object as in
#'   \code{json_object$retweet_status}.
#' @import dplyr
.parse_retweet <- function(x) {
  .extend_label_df(parse_status(x), "retweet")
}


#' parse_all_tweets
#'
#' @description Converts Twitter status object to neat data_frame.
#'   This function subsumes the parse_status function and also
#'   converts place and user objects.
#'
#' @param x json response object from tweet/status
#'   Twitter API request.
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#'
#' @return data_frame
#' @import dplyr
#' @export
parse_all_tweets <- function(x) {
  tweets_df <- parse_status(x)

  if (is.data.frame(x$place)) {
    tweets_df <- bind_cols(
      tweets_df, .parse_place(x$place))
  }

  if (is.data.frame(x$user)) {
    tweets_df <- bind_cols(
      tweets_df, parse_user(x$user))
  }

  if (is.data.frame(x$retweeted_status)) {
    tweets_df <- bind_cols(
      tweets_df, .parse_retweet(x$retweeted_status))
  }

  tweets_df
}


#' .parse_place
#'
#' @param x json resposne object from user lookup Twitter
#'   API call.
#' @import dplyr
.parse_place <- function(x) {
  place_df <- data_frame(
    "place_id" = x$id,
    "place_url" = x$url,
    "place_type" = x$place_type,
    "place_name" = x$name,
    "place_full_name" = x$full_name,
    "place_country_code" = x$country_code,
    "place_country" = x$country,
    "place_long1" = lapply(x$bounding_box$coordinates,
      function(x) x[1, 1, 1]),
    "place_long2" = lapply(x$bounding_box$coordinates,
      function(x) x[1, 2, 1]),
    "place_long3" = lapply(x$bounding_box$coordinates,
      function(x) x[1, 3, 1]),
    "place_long4" = lapply(x$bounding_box$coordinates,
      function(x) x[1, 4, 1]),
    "place_lat1" = lapply(x$bounding_box$coordinates,
      function(x) x[1, 1, 2]),
    "place_lat2" = lapply(x$bounding_box$coordinates,
      function(x) x[1, 2, 2]),
    "place_lat3" = lapply(x$bounding_box$coordinates,
      function(x) x[1, 3, 2]),
    "place_lat4" = lapply(x$bounding_box$coordinates,
      function(x) x[1, 4, 2]))

  place_df
}

#' .extend_label_df
#'
#' @param dff Extended data.frame within larger json
#'   response object
#' @param label New label to represent the other data.frame
#' @import dplyr
.extend_label_df <- function(dff, label = "other") {
  names(dff) <- vapply(
    names(dff),
    function(x) paste0(label, "_", unlist(x)),
    FUN.VALUE = vector("character", 1),
    USE.NAMES = FALSE)

  dff
}

#' parse_user
#'
#' @description Converts Twitter user object to neat data_frame.
#' @param x json resposne object from user lookup Twitter API call.
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#'
#' @return data frame
#' @import dplyr
#' @export
parse_user <- function(x) {
  user_df <- data_frame(
    "user_id" = x$id_str,
    "name" = x$name,
    "screen_name" = x$screen_name,
    "location" = x$location,
    "description" = x$description,
    "url" = x$url,
    "protected" = x$protected,
    "followers_count" = x$followers_count,
    "friends_count" = x$friends_count,
    "listed_count" = x$listed_count,
    "created_at" = as.POSIXct(
      x$created_at,
      format = "%a %b %d %H:%M:%S %z %Y"),
    "favourites_count" = x$favourites_count,
    "utc_offset" = x$utc_offset,
    "time_zone" = x$time_zone,
    "geo_enabled" = x$geo_enabled,
    "verified" = x$verified,
    "statuses_count" = x$statuses_count,
    "lang" = x$lang)

  user_df
}


#' .prep_list
#'
#' @param x data to be vectorized
.prep_list <- function(x) {

  if (!is.null(dim(x))) return(x)

  ncols <- max(unlist(lapply(x, length)), na.rm = TRUE)

  if (ncols == 1) {
    return(x)
  }
  x <- lapply(x, unname)
  x <- lapply(x, function(x)
    c(x, rep(NA_character_, ncols - length(x))))

  x <- data.frame(matrix(unlist(x), ncol = ncols, byrow = TRUE),
    stringsAsFactors = FALSE)

  names(x) <- sapply(seq_len(ncols), function(x) paste0(".", x))

  x
}

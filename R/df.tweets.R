#' parse_status
#'
#' @param x json object from search tweets
#' @details dplyr
#' @export
parse_status <- function(x) {
  if (sum(x$is_quote_status) == 0) {
    x$quoted_status_id_str <- NA
  }
  df <- dplyr::data_frame(
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

#' parse_retweet
#'
#' @param x json response object as in
#' json_object$retweet_status
#' @seealso See \url{https://dev.twitter.com/overview/
#' documentation}
#' for more information on using Twitter's API.
#' @return data_frame
#' @details dplyr
#' @export
parse_retweet <- function(x) {
  extend_label_df(parse_status(x), "retweet")
}


#' parse_all_tweets
#'
#' @param x json response object from tweet/status
#' Twitter API request.
#' @seealso See \url{https://dev.twitter.com/overview/
#' documentation}
#' for more information on using Twitter's API.
#' @return data_frame
#' @details dplyr
#' @export
parse_all_tweets <- function(x) {
  tweets_df <- parse_status(x)

  if (is.data.frame(x$place)) {
    tweets_df <- dplyr::bind_rows(
      tweets_df, parse_place(x$place))
  }

  if (is.data.frame(x$user)) {
    tweets_df <- dplyr::bind_rows(
      tweets_df, parse_user(x$user))
  }

  if (is.data.frame(x$retweeted_status)) {
    tweets_df <- dplyr::bind_rows(
      tweets_df, parse_retweet(x$retweeted_status))
  }

  tweets_df
}


#' parse_place
#'
#' @param x json resposne object from user lookup Twitter
#' API call.
#' @seealso See \url{https://dev.twitter.com/overview/
#' documentation} for more information on using
#' Twitter's API.
#' @return data frame
#' @details dplyr
#' @export
parse_place <- function(x) {
  place_df <- dplyr::data_frame(
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

#' extend_label_df
#'
#' @param dff other, extended data.frame within larger json
#' response object
#' @param new label to represent the other data.frame
#' @seealso See \url{https://dev.twitter.com/overview/documentation}
#' for more information on using Twitter's API.
#' @return data_frame
#' @details dplyr
#' @export
extend_label_df <- function(dff, label = "other") {
  names(dff) <- vapply(
    names(dff),
    function(x) paste0(label, "_", unlist(x)),
    FUN.VALUE = vector("character", 1),
    USE.NAMES = FALSE)

  dff
}

#' parse_user
#'
#' @param x json resposne object from user lookup Twitter API call.
#' @seealso See \url{https://dev.twitter.com/overview/documentation}
#' for more information on using Twitter's API.
#' @return data frame
#' @details dplyr
#' @export
parse_user <- function(x) {
  user_df <- dplyr::data_frame(
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
      as.numeric(x$created_at),
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
#' @export
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

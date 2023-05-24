
#' Fields
#'
#' Arguments of expansion that select which values are returned.
#' Fields are possible for:
#'  - [Tweets](https://developer.twitter.com/en/docs/twitter-api/data-dictionary/object-model/tweet)
#'  - [Users](https://developer.twitter.com/en/docs/twitter-api/data-dictionary/object-model/user)
#'  - [Media](https://developer.twitter.com/en/docs/twitter-api/data-dictionary/object-model/media)
#'  - [Polls](https://developer.twitter.com/en/docs/twitter-api/data-dictionary/object-model/poll)
#'  - [Places](https://developer.twitter.com/en/docs/twitter-api/data-dictionary/object-model/place)
#'  - [Lists](https://developer.twitter.com/en/docs/twitter-api/data-dictionary/object-model/lists)
#' @references <https://developer.twitter.com/en/docs/twitter-api/fields>
#' @seealso [Expansions], [set_fields()]
#' @name Fields
#' @aliases fields
#' @examples
#' media_fields
#' place_fields
#' poll_fields
#' tweet_fields
#' user_fields
NULL

#' @export
#' @name Fields
media_fields <- c("duration_ms", "height", "media_key", "preview_image_url",
                  "type", "url",
    "width", "public_metrics", "non_public_metrics", "organic_metrics",
    "promoted_metrics", "alt_text", "variants")
#' @export
#' @name Fields
place_fields <- c("contained_within", "country", "country_code",
                  "full_name", "geo", "id", "name", "place_type")
#' @export
#' @name Fields
poll_fields <- c("duration_minutes", "end_datetime", "id", "options",
                 "voting_status")

#' @export
#' @name Fields
list_fields <- c("created_at", "follower_count", "member_count",
                 "private", "description", "owner_id")

#' @export
#' @name Fields
tweet_fields <- c(
  "attachments", "author_id", "context_annotations",
  "conversation_id", "created_at", "entities", "edit_controls",
  "geo", "id", "in_reply_to_user_id", "lang", "public_metrics",
  "possibly_sensitive", "referenced_tweets", "reply_settings",
  "source", "text", "withheld"
  # Even with academic permissions this doesn't work (organic metrics might need my own tweets)
  # "non_public_metrics",
  # "promoted_metrics",
  # "organic_metrics" #needs user context authentication.
)

#' @export
#' @name Fields
user_fields <- c("created_at", "description", "entities", "id", "location",
                 "verified_type", "name", "pinned_tweet_id", "profile_image_url",
                 "protected", "public_metrics", "url", "username", "verified",
                 "withheld")

#' @export
#' @name Fields
metrics_fields <- c("public_metrics", "non_public_metrics", "organic_metrics", "promoted_metrics")

#' Create fields
#'
#' Choose which fields are used, by default all are returned. Usually all
#' the first 3 are accepted together and the last two too.
#'
#' @param media The fields you want from `media_fields`.
#' @param poll The fields you want from `poll_fields`.
#' @param tweet The fields you want from `tweet_fields`.
#' @param place The fields you want from `place_fields`.
#' @param user The fields you want from `user_fields`.
#' @param list The fields you want from `list_fields`.
#' @return A list with the fields requested ready to be used in your requests to
#' the API.
#' @seealso Fields
#' @export
#' @examples
#' set_fields()
#' set_fields(media = NULL)
#' set_fields(place = NULL, user = NULL)
set_fields <- function(media = media_fields,
                       poll = poll_fields,
                       tweet = tweet_fields,
                       place = place_fields,
                       user = user_fields,
                       list = list_fields) {

  if (length(media)  == 1 && is.na(media)) {
    media <- NULL
  }
  if (length(place)  == 1 && is.na(place)) {
    place <- NULL
  }
  if (length(poll)  == 1 && is.na(poll)) {
    poll <- NULL
  }
  if (length(tweet)  == 1 && is.na(tweet)) {
    tweet <- NULL
  }
  if (length(user)  == 1 && is.na(user)) {
    user <- NULL
  }


  fields <- list("media.fields" = media, "place.fields" = place,
                 "poll.fields" = poll, "tweet.fields" = tweet,
                 "user.fields" = user, "list.fields" = list)
  if (sum(lengths(fields) > 0) == 0) {
    return(NULL)
  }
  fields <- fields[lengths(fields) > 0]

  fields_c <- vapply(fields, is.character, logical(1L))
  if (any(!fields_c)) {
    abort("There is a field without characters.", call = current_call())
  }

  error <- c(
    check_field_helper(fields, media_fields, "media.fields"),
    check_field_helper(fields, place_fields, "place.fields"),
    check_field_helper(fields, poll_fields, "poll.fields"),
    check_field_helper(fields, tweet_fields, "tweet.fields"),
    check_field_helper(fields, user_fields, "user.fields"),
    check_field_helper(fields, list_fields, "list.fields")
  )
  if (!is.null(error)) {
    abort(error)
  }

  fields
}


# To disable a field use NULL
check_fields <- function(fields,
                         media = media_fields,
                         place = place_fields,
                         poll = poll_fields,
                         tweet = tweet_fields,
                         user = user_fields,
                         list = list_fields,
                         metrics = metrics_fields) {

  # If null, empty list or NA return NULL to disable the fields
  empty_list <- is.list(fields) && length(fields) == 0
  na <- length(fields) == 1L && is.na(fields)
  if ( is.null(fields) || empty_list || na) {
    return(NULL)
  }

  # Check the fields on each one:
  n_fields <- names(fields)
  valid_fields <- c("media", "place", "poll", "tweet", "user", "list", "metrics")
  valid_fields <- paste0(valid_fields, ".fields")

  if (length(setdiff(n_fields, valid_fields)) >= 1) {
    warn("Invalid fields provided, they are omitted")
  }

  error <- c(
    check_field_helper(fields, media, "media.fields"),
    check_field_helper(fields, place, "place.fields"),
    check_field_helper(fields, poll, "poll.fields"),
    check_field_helper(fields, tweet, "tweet.fields"),
    check_field_helper(fields, user, "user.fields"),
    check_field_helper(fields, metrics, "metrics.fields")
  )
  if (!is.null(error)) {
    abort(error, call = current_call())
  }

  fields <- fields[intersect(n_fields, valid_fields)]
  fields
}


check_field_helper <- function(passed, allowed, name) {
  y <- passed[[name]]
  if (is.null(allowed) && !is.null(y)) {
    return(c("x" = paste0("Invalid ", name, ".\n")))
  }
  wrong <- setdiff(y, allowed)
  if (length(wrong) >= 1) {
    c("x" = paste0("Invalid fields in ", name,": ",
                   paste(wrong, collapse = ", "), ".\n"))
  } else {
    return(NULL)
  }

}

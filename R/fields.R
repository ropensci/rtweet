
#' Fields
#'
#' Arguments of expansion that select which values are returned.
#' Fields are possible for:
#'  - [Tweets](https://developer.twitter.com/en/docs/twitter-api/data-dictionary/object-model/tweet)
#'  - [Users](https://developer.twitter.com/en/docs/twitter-api/data-dictionary/object-model/user)
#'  - [Media](https://developer.twitter.com/en/docs/twitter-api/data-dictionary/object-model/media)
#'  - [Polls](https://developer.twitter.com/en/docs/twitter-api/data-dictionary/object-model/poll)
#'  - [Places](https://developer.twitter.com/en/docs/twitter-api/data-dictionary/object-model/place)
#' @references <https://developer.twitter.com/en/docs/twitter-api/fields>
#' @seealso [Expansions]
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
tweet_fields <- c(
  "attachments", "author_id", "context_annotations",
  "conversation_id", "created_at", "entities",
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
                  "name", "pinned_tweet_id", "profile_image_url", "protected",
                  "public_metrics", "url", "username", "verified", "withheld")

#' @export
#' @name Fields
metrics_fields <- c("public_metrics", "non_public_metrics", "organic_metrics", "promoted_metrics")


check_fields <- function(fields,
                         media_fields = NULL,
                         place_fields = NULL,
                         poll_fields = NULL,
                         tweet_fields = NULL,
                         user_fields = NULL,
                         metrics_fields = NULL) {

  # If null use all the allowed fields
  if (is.null(fields)) {
    fields <- list("media.fields" = media_fields, "place.fields" = place_fields,
                   "poll.fields" = poll_fields, "tweet.fields" = tweet_fields,
                   "user.fields" = user_fields, "metrics.fields" = metrics_fields)
    return(fields[lengths(fields) > 0])
  }
  # Empty or NA return NULL to disable the field
  empty_list <- is.list(fields) && length(fields) == 0
  na <- length(fields) == 1L && is.na(fields)
  if ( empty_list || na) {
    return(NULL)
  }

  # Check the fields on each one:
  n_fields <- names(fields)
  valid_fields <- c("media", "place", "poll", "tweet", "user", "metrics")
  valid_fields <- paste0(valid_fields, ".fields")

  if (length(setdiff(n_fields, valid_fields)) >= 1) {
    warning("Invalid fields provided, they are omitted", call. = FALSE)
  }

  error <- c(
    check_field_helper(fields, media_fields, "media"),
    check_field_helper(fields, place_fields, "place"),
    check_field_helper(fields, poll_fields, "poll"),
    check_field_helper(fields, tweet_fields, "tweet"),
    check_field_helper(fields, user_fields, "user"),
    check_field_helper(fields, metrics_fields, "metrics")
  )
  if (!is.null(error)) {
    stop(error, call. = FALSE)
  }

  fields <- fields[intersect(n_fields, valid_fields)]
  fields
}


check_field_helper <- function(passed, allowed, name) {
  y <- passed[[name]]
  if (is.null(allowed) && !is.null(y)) {
    return(paste0("No ", name, " field allowed"))
  }
  wrong <- setdiff(y, allowed)
  if (length(wrong) >= 1) {
    paste("Fields", paste(wrong, collapse = ", "), "are not allowed or valid.\n")
  } else {
    return(NULL)
  }

}


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
metrics_fields <- c("public_metrics", "non_public_metrics", "organic_metrics", "promoted_metrics")

#' Expansions
#'
#' Twitter parameters to add more fields on the returned values.
#' Main ones:
#'  - Tweet
#'    - Referenced tweets
#'    - Attachments
#'  - User
#' @param attachments Add attachments values? Default yes.
#' @param referenced_tweets Add referenced_tweets values? Default yes.
#' @return A character with the characters of valid expanions.
#' @references <https://developer.twitter.com/en/docs/twitter-api/expansions>
#' @seealso [Fields]
#' @name Expansions
#' @aliases expansions
#' @examples
#' tweet_expansions()
#' user_expansions()
#' @export
tweet_expansions <- function(attachments = TRUE, referenced_tweets = TRUE) {
  expansions <- c("author_id", "in_reply_to_user_id", "geo.place_id",
                  "entities.mentions.username",  "edit_history_tweet_ids")
  if (attachments) {
    expansions <- c(expansions, "attachments.media_keys",
                    "attachments.poll_ids")
  }
  if (referenced_tweets) {
    expansions <- c(expansions, "referenced_tweets.id", "referenced_tweets.id.author_id")
  }
  expansions
}

#' @export
#' @name Expansions
user_expansions <- function() {
  "pinned_tweet_id"
}

check_expansions <- function(passed, allowed) {
  within <- passed %in% allowed
  if (length(passed) > 10 || any(within)) {
    extensions <- passed[within]
    stop("These extensions are now allowed: ",
         paste(extensions, collapse = ", "), call. = FALSE)
  }
}

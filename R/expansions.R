#' Expansions
#'
#' Twitter parameters to add more fields on the returned values.
#'
#' The `set_expansions` can be used to prepare the arguments for other rtweet functions.
#'
#' @param attachments Add attachments values? Default yes.
#' @param referenced_tweets Add referenced_tweets values? Default yes.
#' @param tweet,user,list [tweet_expansions()], [user_expansions()] and [tweet_expansions()].
#' @return A character with the characters of valid expansions.
#' @references <https://developer.twitter.com/en/docs/twitter-api/expansions>
#' @seealso [Fields], [set_fields()]
#' @name Expansions
#' @aliases expansions
#' @examples
#' tweet_expansions()
#' user_expansions()
#' set_expansions()
#' @export
set_expansions <- function(tweet = tweet_expansions(),
                           user = user_expansions(),
                           list = list_expansions()) {

  if (is.numeric(tweet)) {
    abort("Invalid tweet expansions.", call = current_call())
  }
  if (is.numeric(user)) {
    abort("Invalid user expansions.", call = current_call())
  }

  expansions <- c(tweet, user, list)

  check_expansions(expansions)
}

#' @export
#' @name Expansions
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
#' @export
#' @name Expansions
list_expansions <- function() {
  "owner_id"
}

check_expansions <- function(passed,
                             allowed = c(tweet_expansions(), user_expansions(), list_expansions())) {
  # Empty list or NA return NULL to disable the expansions
  empty_list <- is.list(passed) && length(passed) == 0
  na <- length(passed) == 1L && is.na(passed)
  if (is.null(passed) || empty_list || na) {
    return(NULL)
  }

  within <- !passed %in% allowed
  if (any(within)) {
    extensions <- passed[within]
    abort(paste0("These extensions are not allowed: ",
         paste(extensions, collapse = ", ")), call = current_call())
  }
  passed
}

# According to https://developer.twitter.com/en/docs/twitter-api/tweets/lookup/api-reference/get-tweets
# attachments.media_keys is required for any media.fields (go to includes)
# geo.place_id is required for any place.fields (go to includes)
# attachments.poll_ids is required by any poll.fields (go to includes)
# referenced_tweets.id is required by any tweet.fields (go to includes)
# author_id, entities.mentions.username, in_reply_to_user_id, referenced_tweets.id.author_id is required by any user.fields (go to includes)
expansions_for_fields <- function(expansion, fields) {
  # Empty fields but might be expansions: no problem
  if (is.null(fields)) {
    return(TRUE)
  }
  msg <- c("Missing expansions for the fields provided.",
           "i" = "Add to expansions:")
  msg2 <- c_f_e(expansion, "attachments.media_keys", fields, "media.fields")
  msg3 <- c_f_e(expansion, "geo.place_id", fields, "place.fields")
  msg4 <- c_f_e(expansion, "attachments.poll_ids", fields, "poll.fields")
  # if (!is.null(fields[["tweet.fields"]]) && "referenced_tweets.id" %in% expansion) {
  #   problem <- TRUE
  # }
  a <- c("author_id", "entities.mentions.username", "in_reply_to_user_id",
         "referenced_tweets.id.author_id", "owner_id")
  if (!is.null(fields[["user.fields"]]) && !any(a %in% expansion)) {
    msg5 <- c("*" = paste0("Add at least one of: ",
                           paste0(sQuote(a, "'"), collapse = ", ")))
  } else {
    msg5 <- NULL
  }
  if (length(c(msg2, msg3, msg4, msg5)) >= 1) {
    abort(c(msg, msg2, msg3, msg4, msg5), call = current_call())
  }
  TRUE
}

# Check fields and expansions
c_f_e <- function(expansions, e, fields, f) {
  if (is.null(fields[[f]])) {
    return(NULL)
  }
  if (!all(e %in% expansions) || is.null(expansions)) {
    f <- gsub("\\.fields", "", f, fixed = TRUE)
    return(c("*" = sQuote(e, "'")))
  }
  return(NULL)
}

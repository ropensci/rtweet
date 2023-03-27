all_scopes <- c("tweet.read", "users.read", "follows.read", "space.read",
                "mute.read", "like.read", "block.read", "bookmark.read",
                "list.read",
                "tweet.write", "tweet.moderate.write", "follows.write", "offline.access", "mute.write", "like.write",
                "list.write",  "block.write",  "bookmark.write"
)

endpoints <- c("tweet", "users", "follows", "space", "mute", "like", "list", "block", "bookmark")

#' Scopes of the OAuth2 token
#'
#' Permissions given to a token of a Twitter account.
#' By default it allows everything.
#' @export
#' @param read Allow to read.
#' @param write Allow to write/manage?
#' @param tweet_moderate Allow to hide or show replies to your Tweets.
#' @param regenerate Allow to use the token for more than 2 hours.
#' @returns A character with all the possible scopes or those allowed.
#' @references <https://developer.twitter.com/en/docs/authentication/oauth-2-0/authorization-code>
#' @examples
#' set_scopes()
set_scopes <- function(read = TRUE, write = TRUE, tweet_moderate = TRUE, regenerate = TRUE) {
  scopes <- c()
  if (isTRUE(read) && isTRUE(write)) {
    scopes <- c(scopes, all_scopes)
  }
  if (isTRUE(read) && is.null(endpoints)) {
    scopes <- all_scopes[endsWith(all_scopes, ".read")]
  }
  if (isTRUE(write) && is.null(endpoints)) {
    scopes <- all_scopes[endsWith(all_scopes, ".write")]
  } else if (isTRUE(write)) {

  }
  if (isTRUE(tweet_moderate)) {
    scopes <- c(scopes, "tweet.moderate")
  }
  # To allow to regenerate the token we need to provide the offline.access:
  if (isTRUE(regenerate)) {
    scopes <- c(scopes, "offline.access")
  }
  scopes
}

get_scopes <- function(token, call = caller_env()) {
  token <- check_token_v2(token, "pkce", call)
  strsplit(token$scope, " ")[[1]]
}

check_scopes <- function(scopes, required = NULL, call = caller_env()) {
  if (is.null(required)) {
    diff <- setdiff(scopes, all_scopes)
    if (length(diff) != 0) {
      msg <- paste0("Scopes required are not valid: ",
                    paste0(sQuote(diff), collapse = ", "))
      abort(msg, call = call)
    }
  }
  missing <- setdiff(required, scopes)
  if (length(missing) != 0) {
    msg <- c("This endpoint requires missing scopes.",
             paste0("Authenticate with scopes:", paste0(sQuote(missing), collapse = ", ")))
    abort(msg, call = call)
  }
  TRUE
}

check_scopes_token <- function(token, required, call = caller_env()) {
  if (!auth_is_pkce(token)) {
    return(TRUE)
  }
  check_scopes(required, call = call)
  check_scopes(get_scopes(token), required, call = call)
  TRUE
}

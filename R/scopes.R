all_scopes <- c("tweet.read", "tweet.write", "tweet.moderate.write", "users.read",
                "follows.read", "follows.write", "offline.access", "space.read",
                "mute.read", "mute.write", "like.read", "like.write", "list.read",
                "list.write", "block.read", "block.write", "bookmark.read", "bookmark.write"
)


#' Scopes of the OAuth client
#'
#' Permissions given to a client of a Twitter account.
#' @export
#' @param token An authentication mechanism. Preferably set via [auth_as()]
#' @returns A character with all the possible scopes.
#' @examples
#' set_scopes(NULL)
set_scopes <- function(token = NULL) {
  if (is.null(token)) {
    return(all_scopes)
  }
  get_scopes(token)
}

get_scopes <- function(token, call = caller_env()) {
  token <- check_token_v2(token, "pkce", call)
  strsplit(token$scope, " ")[[1]]
}

# These functions can be simplified:
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
    msg <- paste0("This endpoint requires missing ",
                  paste0(sQuote(missing), collapse = ", "), " scopes")
    abort(msg, call = call)
  }
  TRUE
}

check_scopes_token <- function(token, required) {
  stopifnot(check_scopes(required))
  if (!auth_is_pkce(token)) {
    return(TRUE)
  }
  scopes <- get_scopes(token)
  if (is.null(scopes)) {
    abort(c("The authentication used must have scopes.",
            i = paste0("Authenticate with scopes:",
                       paste0(sQuote(required), collapse = ", "))))
  } else if (!check_scopes(scopes, required)) {
    missing_scopes <- setdiff(required, scopes)
    abort(c("Missing scopes for this endpoint:",
            i = paste0("Authenticate with:", sQuote(missing_scopes))))
  }
  TRUE
}

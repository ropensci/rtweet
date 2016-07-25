#' from_js
#'
#' @param rsp json object
#' @import httr
#' @details jsonlite
#' @export
from_js <- function(rsp) {
  if (http_type(rsp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  jsonlite::fromJSON(content(rsp, as = "text"))
}

#' .id_type
#'
#' @param x Twitter user id or screen name
#' @return logical value indicating whether object is
#'   screen name [or user ID]
#' @export
.id_type <- function(x) {
  if (suppressWarnings(is.na(as.numeric(x)))) {
    return("screen_name")
  } else {
    return("user_id")
  }
}

#' which_ids
#'
#' Returns integer values. Used for get_friends function.
#' @param n starting number for users
#' @param max_users max number of user ids (if rate limit exceeds
#' remaining number of users, this sets upper ceiling and reduces
#' likelihood of API request errors)
#' @param token Specify token if there is reason to believe
#' current remaning friend list request is below the rate limit
#' max of 15. This rate limit resets every 15 minutes,
#' so this is usually not necessary. Checking rate limits
#' does not reduce the number of available requests, but
#' it does slow things down.
#' @return integers used to identify 15 (or token max given
#' rate limits) users from provided list of user ids
#' @export
which_ids <- function(n, max_users, token = NULL) {
  if (!is.null(token)) {
    total <- check_rate_limit("friends", token)
    if (total == 0) {
      return(invisible())
    }
  } else {
    total <- 15
  }
  remain <- total - 1

  n <- n * (remain + 1) - remain
  end <- n + remain

  if (!missing(max_users)) {
    if (end > max_users) {
      end <- max_users
    }
  }

  n:end
}

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

#' rate_limit
#'
#' @param token An OAuth token (1.0 or 2.0)
#' @param query If null, returns entire rate limit request object as
#'   data frame. otherwise, query returns specific values matching
#'   the query of interest; e.g., \code{query = "lookup"} returns
#'   remaining limit for user lookup requests;
#'   \code{type = "followers"} returns remaining limit for
#'   follower id requests; \code{type = "friends"} returns
#'   remaining limit for friend id requests.
#' @seealso See \url{https://dev.twitter.com/overview/documentation}
#'   for more information on using Twitter's API.
#' @return response Rate limit response object or specific value of
#'   remaining requests
#' @export
rate_limit <- function(token, query = NULL, rest = TRUE) {

  url <- make_url(restapi = rest,
    query = "application/rate_limit_status",
    param = list(resources = "users,statuses,friends,search"))

  r <- TWIT(get = TRUE, url, config = token, catch_error = FALSE)

  rl_df <- as.data.frame(from_js(r), stringsAsFactors = FALSE)
  rl_df <- rl_df[!names(rl_df) == "access_token"]

  rl_df <- data.frame(
    query = gsub(
      "resources.users..|resources.statuses..|resources.friends..|resources.search..",
      "", unique(gsub(
        ".limit$|.reset$|.remaining$|", "",
        names(rl_df)))),
    limit =  unlist(rl_df[seq(1, length(rl_df), 3)]),
    remaining = unlist(rl_df[seq(2, length(rl_df), 3)]),
    reset = unlist(rl_df[seq(3, length(rl_df), 3)]),
    stringsAsFactors = FALSE,
    row.names = NULL)

  rl_df$reset <- as.POSIXct(
    as.numeric(rl_df$reset),
    origin = "1970-01-01")

  if (!is.null(query)) {
    query <- gsub("[/]", ".", query)
    rl_df <- rl_df[grep(query, rl_df$query), ]
    row.names(rl_df) <- NULL
  }

  rl_df
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
    total <- rate_limit(token, "friends/ids")
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

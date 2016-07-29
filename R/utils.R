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

  url <- make_url(
    restapi = rest,
    query = "application/rate_limit_status")

  r <- TWIT(get = TRUE, url, config = token)

  rl_df <- rl_df(r)

  if (!is.null(query)) {
    rl_df <- rl_df[grep(query, rl_df$query), ]
    row.names(rl_df) <- NULL
  }

  rl_df
}

#' rl_df
#'
#' Returns integer values. Used for get_friends function.
#' @param r rate limit response object
#' @return cleaned up data frame of rate limit info
rl_df <- function(r) {
  r <- from_js(r)
  data <- r$resources

  rl_df <- data.frame(
    query = gsub(".limit|.remaining|.reset", "",
      gsub(".*[.][/]", "", names(unlist(data)))),
    limit = unlist(lapply(data, function(x)
      unlist(lapply(x, function(y) y['limit'])))),
    remaining = unlist(lapply(data, function(x)
      unlist(lapply(x, function(y) y['remaining'])))),
    reset = unlist(lapply(data, function(x)
      unlist(lapply(x, function(y) y['reset'])))),
    row.names = NULL,
    stringsAsFactors = FALSE)

  rl_df <- rl_df[!duplicated(rl_df$query), ]

  row.names(rl_df) <- NULL

  rl_df$reset <- difftime(
    as.POSIXct(rl_df$reset,
      origin = "1970-01-01"),
    Sys.time(),
    units = "mins")

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

#' stream_params
#'
#' Returns stream param.
#' @param stream character stream query
#' @return param character vector
stream_params <- function(stream) {
  stream <- unlist(trimws(unlist(strsplit(stream, ","))))

  if (!all(suppressWarnings(is.na(as.numeric(stream))))) {
    if (all(is.integer(as.integer(stream)))) {
      params <- list(follow = stream)
    } else {
      params <- list(locations = stream)
    }
  } else {
    params <- list(track = stream)
  }
  params
}

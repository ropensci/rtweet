#' .from_js
#'
#' @keywords internal
#' @param rsp json object
#' @import httr jsonlite
.from_js <- function(rsp) {
  if (http_type(rsp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  fromJSON(content(rsp, as = "text"))
}

#' .id_type
#'
#' @keywords internal
#' @param x Twitter user id or screen name
#' @return Character vector of either screen_name or user_id
.id_type <- function(x) {
  if (suppressWarnings(is.na(as.numeric(x)))) {
    return("screen_name")
  } else {
    return("user_id")
  }
}

#' rate_limit
#'
#' @description Returns rate limit information for Twitter
#'   access tokens.
#'
#' @param token OAuth token (1.0 or 2.0). By default
#'   \code{token = NULL} fetches a non-exhausted token from
#'   an environment variable.
#' @param query If null, returns entire rate limit request object as
#'   data frame. otherwise, query returns specific values matching
#'   the query of interest; e.g., \code{query = "lookup/users"} returns
#'   remaining limit for user lookup requests;
#'   \code{type = "followers/ids"} returns remaining limit for
#'   follower id requests; \code{type = "friends/ids"} returns
#'   remaining limit for friend id requests.
#' @param rest Logical indicating whether to send request to REST
#'   API. At this time, this should always be TRUE.
#' @seealso \url{https://dev.twitter.com/overview/documentation}
#'
#' @return Data frame with rate limit respones details. If query
#'   is specified, only relevant rows are returned.
#' @export
rate_limit <- function(token, query = NULL, rest = TRUE) {

  url <- make_url(
    restapi = rest,
    query = "application/rate_limit_status")

  r <- TWIT(get = TRUE, url, config = token)

  rl_df <- .rl_df(r)

  if (!is.null(query)) {
    rl_df <- rl_df[grep(query, rl_df$query), ]
    row.names(rl_df) <- NULL
  }

  rl_df
}

#' .rl_df
#'
#' @keywords internal
#' @param r Data frame response object from rate limit TWIT request.
#' @return Oranized data frame of rate limit info
.rl_df <- function(r) {

  r <- .from_js(r)

  data <- r$resources

  rl_df <- data.frame(
    query = gsub(".limit|.remaining|.reset", "",
      gsub(".*[.][/]", "", grep(".limit$", names(unlist(data)), value = TRUE))),
    limit = unlist(lapply(data, function(y)
      lapply(y, function(x) getElement(x, "limit")))),
    remaining = unlist(lapply(data, function(y)
      lapply(y, function(x) getElement(x, "remaining")))),
    reset = unlist(lapply(data, function(y)
      lapply(y, function(x) getElement(x, "reset")))),
    row.names = NULL,
    stringsAsFactors = FALSE)

  rl_df$reset <- difftime(
    as.POSIXct(rl_df$reset,
      origin = "1970-01-01"),
    Sys.time(),
    units = "mins")

  rl_df
}


#' Returns integer values. Used for get_friends function.
#' @keywords internal
.which_ids <- function(n, max_users, token = NULL) {
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

#' .stream_params
#'
#' @keywords internal
#' @description Returns stream param.
#' @param stream character stream query
#' @return param character vector
.stream_params <- function(stream) {
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
